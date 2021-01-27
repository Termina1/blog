{-# LANGUAGE FlexibleInstances #-}

module Data.NoteRemote
  (
    MonadNoteRemote(..)
  ) where

import Models (Note(..))
import Control.Monad.IO.Class (liftIO)
import Data.Config (MonadConfig(..))
import System.FilePath.Posix (joinPath, takeBaseName)
import System.IO.Error (IOError)
import Control.Exception as E (try)
import App (App)
import System.Directory (listDirectory)
import Data.Maybe (catMaybes)
import System.Posix.Files (FileStatus, getFileStatus)
import Renderers.Markdown (createNote)

class (Monad m) => MonadNoteRemote m where
  getNoteByNameRemote :: String -> m (Either String (Maybe Note))
  listNotesRemote :: m (Either String [Note])

instance MonadNoteRemote App where
  getNoteByNameRemote name = do
    folder <- getNotesFolder
    let filename = (joinPath [folder, (name ++ ".md")])
    result <- liftIO $ E.try $ readFile filename
    status <- liftIO $ E.try $ getFileStatus filename
    return $ createNote name status result

  listNotesRemote = do
    folder <- getNotesFolder
    files <- liftIO $ listDirectory folder
    let filesFiltered = filter (\el -> (takeBaseName el) /= "about") files
    fmap (fmap catMaybes) $ fmap (sequence) $ mapM (getNoteByNameRemote . takeBaseName) filesFiltered

-- notFoundNote :: Either IOError String -> Either String String
-- notFoundNote err = if isDoesNotExistError err then Right Nothing else Left (show err)