import Data.Maybe
import Data.Monoid
import System.Environment
import XMonad
import XMonad.Actions.UpdateFocus
import XMonad.Config.Desktop
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP()
import XMonad.Hooks.DynamicProperty
import XMonad.Layout.LayoutModifier
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Simplest
import XMonad.Layout.Dishes
import XMonad.Layout.Spacing
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
    sequence_ [ spawn "notify-send XMonad \"Restarting...\""
              --, killAllStatusBars
              --, spawnStatusBar "xmobar"
              , XMonad.restart (cacheDir dirs ++ "/xmonad-x86_64-linux") True
              ]

-- Recompile Restart
recompileRestart :: X ()
recompileRestart = do
    whenX Main.recompile (sequence_ [ Main.restart, spawn "notify-send XMonad \"Recompiled Successfully!\"" ])
    spawn "notify-send XMonad \"Failed to Recompile\""
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

--styleConfig = {
--              }

--singleSection "#46474F" "#303030" "\57532"
--  . color "#C393D8" "#46474F"

--singleSection styleConfig "\57532"
--    . singleSection colorFg "#C393D8"


ppConfig :: PP
ppConfig = def { ppCurrent = around "#46474F" "#303030" "\57532"
                                . xmobarColor "#C393D8" "#46474F"
               , ppTitle = shorten 100
               , ppSep = "  "
               , ppWsSep = " "
               , ppLayout = around "#46474F" "#303030" "\57532"
                                . xmobarColor "#C393D8" "#46474F"
                                . wrap " " " "
               }

xmobarPath :: MonadIO m => m String
xmobarPath = do
    --home <- liftIO envHome
    --return $ home ++ "/.config/xmobar/xmobar"
    return "xmobar-top"

sb :: StatusBarConfig
sb = def { sbLogHook = logging
         , sbStartupHook = xmobarPath >>= spawnStatusBar
         , sbCleanupHook = xmobarPath >>= killStatusBar
         }

divider :: String -> String -> String -> String
divider = xmobarColor
--divider fromColor toColor = xmobarColor fromColor toColor

around :: String -> String -> String -> String -> String
around bgColorA bgColorB symbol t = concat [ divider bgColorB bgColorA symbol
                                           , t
                                           , divider bgColorA bgColorB symbol
                                           ]

--------------------------------------
-- Workspaces
workspaceId :: Int -> String
workspaceId index = workspaces baseConfig !! index

--------------------------------------

baseConfig :: XConfig (ModifiedLayout AvoidStruts (Choose Tall (Choose (Mirror Tall) Full)))
baseConfig = desktopConfig
    { terminal          = "kitty"
    , modMask           = mod4Mask
    , borderWidth       = 0
    , handleEventHook   = dynamicTitle handleEventsWithDynamicTitle <+> handleEvents
    , workspaces        = map show [1..9 :: Int]
    }

main :: IO ()
main = do
    xmonad $ withSB sb . ewmhFullscreen . docks $ (baseConfig
        { layoutHook = layoutSpecs
        , manageHook = windowManage <+> manageHook baseConfig
        , startupHook = startup
        , logHook = logging
        } `additionalKeysP` keybindings)

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
    , ("M-l", sendMessage NextLayout)
    , ("M-w m", sendMessage Expand)
    , ("M-w n", sendMessage Shrink)

    -- floating layer
    , ("M-w f", withFocused $ windows . W.sink) -- send it back to tiling

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

--layoutSpecs :: ModifiedLayout AvoidStruts (ModifiedLayout WithBorder Full) Window
layoutSpecs = avoidStruts $ -- don't cover status bar
              noBorders $ -- never show borders
              spacingWithEdge 2 $ -- edge spacing
              onWorkspaces (map workspaceId [0, 1]) (Tall 1 (3/100) (4/5) ||| Dishes 1 (1/10) ||| Simplest) $ -- work related workspaces
              Tall 1 (3/100) (4/5) ||| Simplest -- others workspaces

--------------------------------------
-- Window Manage

windowManage :: ManageHook
windowManage = composeAll [ isFullscreen --> doFullFloat
                          --, className =? "panel" --> hasBorder False
                          --, className =? "trayer" -->
                          , isDialog --> doFloat
                          --, manageDocks
                          --, className =? "Spotify" --> doShift (workspaceId 4)
                          --, dynamicPropertyChange "WM_NAME" ()
                          ]

--------------------------------------
-- Logging

logging :: X ()
logging = do
    xmonadPropLog' xmonadDefProp =<< dynamicLogString ppConfig -- =<< def

--------------------------------------
-- Handle Event Hook

runIfClassName :: Monoid a => Window -> String -> X a -> X a
runIfClassName w name action = runQuery (className =? name --> liftX action) w

-- ensure stremio will suspend screensaver
handleEventStremio :: Event -> X ()
handleEventStremio MapRequestEvent {ev_window = w} = runIfClassName w "Stremio" (spawn $ "xdg-screensaver suspend " ++ show w)
handleEventStremio UnmapEvent {ev_window = w} = runIfClassName w "Stremio" (spawn $ "xdg-screensaver resume " ++ show w)
handleEventStremio _ = pure ()

handleEventsWithDynamicTitle :: Query (Endo WindowSet)
handleEventsWithDynamicTitle = composeAll [ title =? "Spotify" --> doShift (workspaceId 4) -- send spotify to workspace 4
                                            ]
handleEvents :: Event -> X All
handleEvents e = composeAll [ handleEventStremio e
                            ] >> return (All True)

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
