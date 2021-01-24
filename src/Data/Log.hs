module Data.Log
  (
    MonadLog(..)
  ) where

import System.Log.FastLogger (toLogStr, withFastLogger, LogType'(LogStdout))
import Control.Monad.IO.Class (liftIO, MonadIO)
import App (App)

class (Monad m, MonadIO m) => MonadLog m where
  logT :: String -> m ()

instance MonadLog App where
  logT str = liftIO $ withFastLogger (LogStdout 4000) (\logger -> logger $ toLogStr (str ++ "\n"))