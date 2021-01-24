module AppEnv
  ( App(..),
    AppEnv(..),
    Env(..),
  ) where

import Control.Monad.Except (ExceptT, MonadError(..), runExceptT)
import Servant.Server (ServerError)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (ReaderT, MonadReader, runReaderT)
import Data.Kind (Type)

data Env (m :: Type -> Type) = Env {
  notesFolder :: !(String),
  ttl :: !(Int)
}

type AppEnv = Env App
newtype App a = App
  { unApp :: ReaderT AppEnv (ExceptT ServerError IO) a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadReader AppEnv, MonadError ServerError)