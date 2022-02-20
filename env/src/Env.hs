module Env (
        retrieve,
        EnvVars(..), vars
    ) where

import Data.Maybe
import System.Environment

--------------------------------------
-- Helpers

retrieve :: String -> IO String
retrieve name = do
    e <- lookupEnv name
    return $ fromMaybe "" e

--------------------------------------
-- Env Variables

data EnvVars = EnvVars { home :: IO String
                       , browser :: IO String
                       }

vars = EnvVars { home = retrieve "HOME"
               , browser = retrieve "BROWSER"
               }
