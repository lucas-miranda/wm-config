import Data.Maybe
import System.Environment
import XMonad
import XMonad.Actions.UpdateFocus
import XMonad.Config.Desktop
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.StatusBar
import XMonad.Layout.LayoutModifier
import XMonad.Layout.NoBorders
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig
import XMonad.Util.Run

--------------------------------------
-- Util

-- Environment
retrieveEnv :: String -> IO String
retrieveEnv name = do
    e <- lookupEnv name
    return $ fromMaybe "" e

-- Spawn
safeSpawnIO :: MonadIO m => IO String -> [IO String] -> m ()
safeSpawnIO x args = do
    liftedArgs <- liftIO $ sequence args
    liftedX <- liftIO x
    safeSpawn liftedX liftedArgs

-- Meta

-- Recompile
recompile :: MonadIO m => m Bool
recompile = do
    dirs <- liftIO getDirectories
    XMonad.recompile dirs True

-- Restart
restart :: X ()
restart = do
    dirs <- liftIO getDirectories
    sequence_ [ spawn "notify-send XMonad Restarting..."
              --, killAllStatusBars
              --, spawnStatusBar "xmobar"
              , XMonad.restart (cacheDir dirs ++ "/xmonad-x86_64-linux") True
              ]

-- Recompile Restart
recompileRestart :: X ()
recompileRestart = do
    whenX Main.recompile (sequence_ [ spawn "notify-send XMonad Recompiled Successfully!", Main.restart ])
    spawn "notify-send XMonad Failed to Recompile"
    def

--------------------------------------
-- Environment Variables

-- home
envHome :: IO String
envHome = retrieveEnv "HOME"

-- browser
envBrowser :: IO String
envBrowser = retrieveEnv "BROWSER"

--------------------------------------
-- Shortcuts

-- browser
spawnBrowser :: MonadIO m => m ()
spawnBrowser = safeSpawnIO envBrowser []

--------------------------------------
-- Xmobar

--ppConfig = def { ppCurrent = xmobarColor "black" "white" }
--
--ppConfig :: PP
--ppConfig = def { ppCurrent          = wrap "[" "]"
               --, ppVisible          = wrap "<" ">"
               --, ppHidden           = id
               --, ppHiddenNoWindows  = const ""
               --, ppVisibleNoWindows = Nothing
               --, ppUrgent           = id
               --, ppRename           = pure
               --, ppSep              = " : "
               --, ppWsSep            = " "
               --, ppTitle            = shorten 80
               --, ppTitleSanitize    = xmobarStrip . dzenEscape
               --, ppLayout           = id
               --, ppOrder            = id
               --, ppOutput           = putStrLn
               ----, ppSort             = getSortByIndex
               --, ppExtras           = []
               ----, ppPrinters         = empty
               --}

xmobarPath :: MonadIO m => m String
xmobarPath = do
    --home <- liftIO envHome
    --return $ home ++ "/.config/xmobar/xmobar"
    return "xmobar-top"

sb :: StatusBarConfig
sb = def { sbLogHook = xmonadPropLog' xmonadDefProp =<< dynamicLogString =<< def
         , sbStartupHook = xmobarPath >>= spawnStatusBar
         , sbCleanupHook = xmobarPath >>= killStatusBar
         }

--------------------------------------

baseConfig :: XConfig (ModifiedLayout AvoidStruts (Choose Tall (Choose (Mirror Tall) Full)))
baseConfig = desktopConfig
    { terminal    = "kitty"
    , modMask     = mod4Mask
    , XMonad.borderWidth = 1
    , normalBorderColor = "#6B6B6B"
    , focusedBorderColor = "#B152FF"
    }

main :: IO ()
main = do
    --dirs <- getDirectories
    --xmonad . ewmh . docks $ (baseConfig
    xmonad . withSB sb . ewmhFullscreen . docks $ (baseConfig
        { layoutHook = layoutSpecs
        , manageHook = windowManage
        , startupHook = startup
        , logHook = logging
        } `additionalKeysP` keybindings) --)
        --dirs

--------------------------------------
-- Keybindings

keybindings :: [(String, X ())]
keybindings =
    -- killing
    [ ("M-q", kill)

    -- xmonad
    , ("S-M-r", Main.restart)
    , ("S-M-c", recompileRestart)

    -- layout
    , ("M-j", windows W.focusDown)
    , ("M-k", windows W.focusUp)
    , ("S-M-j", windows W.swapDown)
    , ("S-M-k", windows W.swapUp)

    -- floating layer
    , ("M-w", withFocused $ windows . W.sink) -- send it back to tiling

    -- common programs
    , ("M-t", spawn $ terminal baseConfig)
    , ("M-b", spawnBrowser)

    -- launcher
    , ("M-r", spawn "rofi -show run")
    , ("M-s", spawn "rofi-pass")
    , ("M-e", spawn "rofi -show emoji -modi emoji")

    -- screen print
    , ("<Print>", spawn "flameshot gui")
    , ("S-<Print>", spawn "flameshot screen -c")

    -- media controls
    , ("<XF86AudioPlay>", spawn "playerctl play-pause")
    , ("<XF86AudioNext>", spawn "playerctl next")
    , ("<XF86AudioPrev>", spawn "playerctl previous")
    , ("M-[", spawn "playerctl next")
    , ("M-]", spawn "playerctl previous")

    -- volume
    , ("<XF86AudioRaiseVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ +5%")
    , ("<XF86AudioLowerVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ -5%")
    , ("<XF86AudioMute>", spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle")
    , ("<XF86AudioMicMute>", spawn "pactl set-source-mute @DEFAULT_SOURCE@ toggle")
    ]

--------------------------------------
-- Layout Specs

layoutSpecs :: ModifiedLayout
                 SmartBorder
                 (ModifiedLayout AvoidStruts (ModifiedLayout WithBorder Full))
                 Window
layoutSpecs = smartBorders . avoidStruts $ noBorders Full

--------------------------------------
-- Window Manage

windowManage :: ManageHook
windowManage = composeAll [ isFullscreen --> doFullFloat
                          --, className =? "panel" --> hasBorder False
                          --, className =? "trayer" -->
                          , isDialog --> doFloat
                          , manageDocks
                          , manageHook baseConfig
                          ]

--------------------------------------
-- Startup

startup :: X ()
startup = do
    startupHook desktopConfig
    home <- liftIO envHome
    spawn (home ++ "/.init")
    -- TODO  verify xmobar return status and display a message if any error occurs
    --safeSpawn "xmobar" [ (home ++ "/.config/xmobar/xmobar.hs") ]
    --home <- liftIO envHome

    --spawn (home ++ "/.local/bin/xmobar")

    -- compile xmobar config
    --spawn ("ghc --make " ++ home ++ "/.config/xmobar/xmobar.hs")

    -- run xmobar
    --killStatusBar (home ++ "/.config/xmobar/xmobar")
    --spawnStatusBar (home ++ "/.config/xmobar/xmobar")

    adjustEventInput

logging :: X ()
logging = do
    xmonadPropLog' xmonadDefProp =<< dynamicLogString =<< def
