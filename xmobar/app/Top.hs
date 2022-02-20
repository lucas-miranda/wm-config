import Control.Exception (fromException, try, bracket, bracket_, throw, finally, SomeException(..))
import Control.Monad
import Control.Monad.IO.Class
import Data.Maybe
import Env;
import System.Environment
import System.IO
import System.Info
import System.Posix.Env (getEnv)
import System.Posix.IO
import System.Posix.Process (executeFile, forkProcess, getAnyProcessStatus, createSession)
import System.Posix.Signals
import System.Posix.Types (ProcessID)
import Xmobar

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

config :: Config
config = defaultConfig { font = "xft:FiraCode Nerd Font Mono-9"
                       , bgColor = "#333438"
                       --, bgColor = "#FF0038"
                       , position = Top
                       , commands = [ Run $ Date "%a %Y-%m-%d <fc=#8BE9FD>%H:%M</fc>" "date" 10
                                    , Run $ DynNetwork ["-t", "<dev>: <rxvbar> | <txvbar>"] 10
                                    --, Run $ Network "enp7s0" ["-L","0","-H","32",
                                                             --"--normal","green","--high","red"] 10
                                    --, Run $ Network "wlp8s0" ["-L","0","-H","32",
                                                             --"--normal","green","--high","red"] 10
                                    , Run $ Cpu ["-L","3","-H","50",
                                                  "--normal","green","--high","red"] 10
                                    , Run $ Memory ["-t","Mem: <usedratio>%"] 10
                                    , Run $ Swap [] 10
                                    , Run $ Com "uname" ["-s","-r"] "" 36000
                                    , Run   XMonadLog
                                    ]
                       , sepChar = "%"
                       , alignSep = "}{"
                       , template = "%XMonadLog% }{ %memory% * %swap% | %dynnetwork% | %date% | %uname%"
                       }

main :: IO ()
main = sequence_ [ startTrayer
                 , xmobar config
                 ]

--------------------------------------

startTrayer :: MonadIO m => m ()
startTrayer = do
    h <- liftIO $ home Env.vars
    spawn $ h ++ "/.config/xmobar/scripts/trayer.sh"
