{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module ServerHandlers
  (
    showNote,
    showAllNotes,
    showAboutMe,
    showAllNotesLayout,
    handleUpdate,
    Layout(..)
  ) where

import Models (Note)
import Data.NoteRemote (MonadNoteRemote(..))
import Data.Log (MonadLog(..))
import Data.Config (MonadConfig(getServerEnv, getAuthorEnv, getBlogEnv, getSecret))
import Control.Monad.Except (MonadError(..))
import Servant.Server (ServerError, errBody, err404, err500, err403)
import Renderers.Layout (Layout(..))
import Data.Context (Context(..), ContextEnv(..))
import Servant (NoContent(..))
import Crypto.Hash.SHA256 (hmaclazy)
import Data.ByteString.Char8 (unpack)
import Data.ByteString.Lazy.Char8 (pack)
import Data.Aeson (Value, encode)
import Text.Hex (encodeHex)
import Data.Text as TT (unpack)
import System.Process (shell, readCreateProcess)
import Control.Monad.IO.Class (MonadIO(..))

withContext :: (MonadConfig m) => a -> m (Context a)
withContext a = do
  senv <- getServerEnv
  authorEnv <- getAuthorEnv
  blogEnv <- getBlogEnv
  return $ Context (ContextEnv senv authorEnv blogEnv) a

showNote:: (MonadNoteRemote m, MonadLog m, MonadError ServerError m, MonadConfig m) => String -> m (Context (Layout Note))
showNote name = do
  result <- getNoteByNameRemote name
  case result of
    Left err -> do
      logT (show err)
      throwError $ err500 { errBody = "Failed to load note" }
    Right mnote -> case mnote of
      Nothing -> throwError $ err404 { errBody = "Can't find this note"}
      Just note -> (withContext $ DefaultLayout note)

showAllNotes :: (MonadNoteRemote m, MonadLog m, MonadError ServerError m, MonadConfig m) => m (Context [Note])
showAllNotes = do
  result <- listNotesRemote
  case result of
    Left err -> do
      logT (show err)
      throwError $ err500 { errBody = "Failed to retrieve notes" }
    Right notes -> (withContext notes)

showAllNotesLayout :: (MonadNoteRemote m, MonadLog m, MonadError ServerError m, MonadConfig m) => m (Context (Layout [Note]))
showAllNotesLayout = do
  notes <- showAllNotes
  return $ fmap DefaultLayout notes

showAboutMe :: (MonadNoteRemote m, MonadLog m, MonadError ServerError m, MonadConfig m) => m (Context (Layout Note))
showAboutMe = do
  showNote "about"

handleUpdate :: (MonadLog m, MonadError ServerError m, MonadConfig m) => Maybe String -> String -> m NoContent
handleUpdate (Just sig) body = do
  secret <- getSecret
  let dataSig = TT.unpack $ encodeHex $ hmaclazy secret (pack body)
  if sig /= ("sha256=" ++ dataSig)
    then throwError $ err403 { errBody = "Incorrect signature" }
    else do
      logT "Updating application"
      out <- liftIO $ readCreateProcess (shell "nohup ./restart.sh &") ""
      logT ("Result: " ++ out)
      return NoContent
handleUpdate Nothing body = throwError $ err403 { errBody = "Incorrect signature" }