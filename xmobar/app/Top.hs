import Control.Exception (fromException, try, bracket, bracket_, throw, finally, SomeException(..))
import Control.Monad
import Control.Monad.IO.Class
import Data.Maybe
import Env;
import Numeric;
import Section;
import SectionDivider;
import SectionGroup;
import System.Environment
import System.IO
import System.Info
import System.Posix.Env (getEnv)
import System.Posix.IO
import System.Posix.Process (executeFile, forkProcess, getAnyProcessStatus, createSession)
import System.Posix.Signals
import System.Posix.Types (ProcessID)
import Xmobar
import SectionGroup (SectionGroupConfig(SectionGroupConfig))

--------------------------------------

uninstallSignalHandlers :: MonadIO m => m ()
uninstallSignalHandlers = liftIO $ do
    installHandler openEndedPipe Default Nothing
    installHandler sigCHLD Default Nothing
    return ()

xfork :: MonadIO m => IO () -> m ProcessID
xfork x = liftIO . forkProcess . finally nullStdin $ do
                uninstallSignalHandlers
                createSession
                x
 where
    nullStdin = do
        fd <- openFd "/dev/null" ReadOnly Nothing defaultFileFlags
        dupTo fd stdInput
        closeFd fd

spawn :: MonadIO m => String -> m ()
spawn x = Control.Monad.void (spawnPID x)

spawnPID :: MonadIO m => String -> m ProcessID
spawnPID x = xfork $ executeFile "/bin/sh" False ["-c", x] Nothing

--------------------------------------
-- Paths

rootPath :: String
rootPath = "$HOME"

rootPath' :: String
rootPath' = "/home/luke"

xmobarPath :: String
xmobarPath = rootPath ++ "/.config/xmobar"

xmobarPath' :: String
xmobarPath' = rootPath' ++ "/.config/xmobar"

scriptsPath :: String
scriptsPath = xmobarPath ++ "/scripts"

scriptsPath' :: String
scriptsPath' = xmobarPath' ++ "/scripts"

--------------------------------------
--     直  睊

config :: Config
config = defaultConfig { font = "xft:FiraCode Nerd Font Mono-9"
                       , additionalFonts = ["xft:Do Hyeon-9", "xft:joypixels-9"]
                       , bgColor = "#303030"
                       , position = Static {xpos = 0, ypos = 0, width = 1920, height = 18}
                       , textOffset = 13
                       , iconOffset = 13
                       , alpha = 255
                       , border = NoBorder
                       , overrideRedirect = True
                       , sepChar = "%"
                       , alignSep = "}{"
                       --, template = "%XMonadLog% }{ <fc=#B8B8B9,#46474f>%memory% * %swap%   %dynnetwork%   %datetime%  <fc=#C393D8,#46474f>%uname%</fc> </fc><fc=#333438,#46474f>\57534</fc><fc=#333438,#333438> %traypad%</fc>"
                       , template = " %XMonadLog% }{ " ++ generateTemplate

                       , commands = [ Run $ DateZone "\61463 <fc=#C393D8>%H:%M</fc> %a, %d de %B de %Y" "pt_BR.UTF-8" "America/Sao_Paulo" "datetime" 10
                                    , Run $ DynNetwork ["-t", "<fc=#61636B><dev></fc> <rxvbar><txvbar>"
                                                       , "-L", "64", "-H", "1000000" -- in bytes
                                                       , "-m", "5", "-W", "10", "-S", "Off"
                                                       , "-h", "#7CC4AF", "-n", "#898DA1", "-l", "#C46C64"
                                                       ] 10
                                    , Run $ Cpu ["-L","3","-H","50",
                                                  "--normal","green","--high","red"] 10
                                    , Run $ Memory ["-t","Mem: <usedratio>%"] 10
                                    , Run $ Swap [] 10
                                    , Run $ Com "uname" ["-n"] "" 36000
                                    , Run $ Com (scriptsPath' ++ "/padding-icon.sh") ["panel"] "traypad" 10
                                    , Run $ Alsa "default" "Master" [ "-t", "<status> <volume>% <volumevbar>"
                                                                    --, "--lowd", "-78.0", "--highd", "-2.87"
                                                                    , "--"
                                                                    , "-H", "90", "-L", "5"
                                                                    , "--on", "on ", "--off", "off"
                                                                    , "-C", "#B8B8B9,#46474F", "-c", "#B8B8B9,#46474F"
                                                                    ]
                                    , Run   XMonadLog
                                    ]
                       }

main :: IO ()
main = sequence_ [ startTrayer
                 , xmobar config
                 ]

--------------------------------------

sectionGroupConfig :: SectionGroupConfig
sectionGroupConfig = SectionGroupConfig
    { anchor = AtRight
    , dividerConfig = DividerConfig { symbol = "\57534"
                                    , spacesAtLeft = 1
                                    , spacesAtRight = 1
                                    }
    , sectionA = SectionConfig { foregroundColor = "#B8B8B9"
                               , backgroundColor = "#303030"
                               }
    , sectionB = SectionConfig { foregroundColor = "#B8B8B9"
                               , backgroundColor = "#46474F"
                               }
    }

generateTemplate :: String
generateTemplate = SectionGroup.simple sectionGroupConfig
    [ "%memory%  %swap%"
    , "%dynnetwork%"
    , "%alsa:default:Master%"
    , "%datetime%"
    , "%uname%"
    , "%traypad%"
    ]

startTrayer :: MonadIO m => m ()
startTrayer = spawn $ unwords
    [ scriptsPath ++ "/trayer.sh"
    , "0x" ++ tintColor
    ] where
        tintColor = definedColor
        definedColor = drop 1 (backgroundColor $ sectionA sectionGroupConfig)
        --invertColor c = (showHex $ fromHex "FFFFFF" - fromHex c) ""
        --fromHex h = fst $ head $ readHex h

