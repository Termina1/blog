module Data.Config
  (
    MonadConfig(..)
  ) where

import App (App, Env(..), ServerEnv, AuthorEnv, BlogEnv)
import Control.Monad.Reader (ask)
import Data.ByteString (ByteString)

class (Monad m) => MonadConfig m where
  getTtl :: m Int
  getNotesFolder :: m String
  getServerEnv :: m ServerEnv
  getAuthorEnv :: m AuthorEnv
  getBlogEnv :: m BlogEnv
  getSecret :: m ByteString

instance MonadConfig App where
  getTtl = ask >>= \env -> return $ ttl env
  getNotesFolder = ask >>= \env -> return $ notesFolder env
  getServerEnv = ask >>= \env -> return $ senv env
  getAuthorEnv = ask >>= \env -> return $ authorEnv env
  getBlogEnv = ask >>= \env -> return $ blogEnv env
  getSecret = ask >>= \env -> return $ secret env