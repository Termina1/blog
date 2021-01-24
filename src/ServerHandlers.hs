{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module ServerHandlers
  (
    showNote,
    showAllNotes,
    showAboutMe,
    showAllNotesLayout,
    Layout(..)
  ) where

import Models (Note)
import Data.NoteRemote (MonadNoteRemote(..))
import Data.Log (MonadLog(..))
import Data.Config (MonadConfig(getServerEnv, getAuthorEnv, getBlogEnv))
import Control.Monad.Except (MonadError(..))
import Servant.Server (ServerError, errBody, err404, err500)
import Renderers.Layout (Layout(..))
import Data.Context (Context(..), ContextEnv(..))

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
