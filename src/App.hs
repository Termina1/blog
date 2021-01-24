{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module App
  ( App(..),
    AppEnv(..),
    Env(..),
    ServerEnv(..),
    BlogEnv(..),
    AuthorEnv(..),
    runApp
  ) where

import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (ReaderT, MonadReader, runReaderT)
import Data.Kind (Type)
import Servant.Server (Handler, ServerError)
import Control.Monad.Except (ExceptT, MonadError(..), runExceptT)

data ServerEnv = ServerEnv {
  domain :: String,
  protocol :: String
}

data AuthorEnv = AuthorEnv {
  authorName :: String,
  email :: String
}

data BlogEnv = BlogEnv {
  blogName :: String
}

data Env (m :: Type -> Type) = Env {
  notesFolder :: !(String),
  ttl :: !(Int),
  senv :: !(ServerEnv),
  authorEnv :: !(AuthorEnv),
  blogEnv :: !(BlogEnv)
}

type AppEnv = Env App
newtype App a = App
  { unApp :: ReaderT AppEnv (ExceptT ServerError IO) a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadReader AppEnv, MonadError ServerError)


runApp :: AppEnv -> App a -> Handler a
runApp env app = do
  result <- liftIO $ runExceptT $ runReaderT (unApp app) env
  case result of
    Left err -> throwError err
    Right result -> return result