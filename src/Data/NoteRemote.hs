{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}

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
import System.Posix.Files (FileStatus, getFileStatus, modificationTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Text.Lazy (Text, pack)
import Text.Parsec.String (Parser)
import Text.Parsec as P (manyTill, many, anyToken, string, runParser, try)
import Data.Bifunctor (first)
import Text.Markdown (markdown, defaultMarkdownSettings)
import Text.Blaze.Html (Html)

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

createNote :: String -> Either IOError FileStatus -> Either IOError String -> Either String (Maybe Note)
createNote name statusE textE = do
  text <- first show textE
  (title, previewText, fullText) <- first show $ runParser noteParser () name text
  status <- first show statusE
  let created = posixSecondsToUTCTime $ realToFrac $ modificationTime status
  return $ Just $ Note{..}

noteParser :: Parser (String, Html, Html)
noteParser = do
  title <- manyTill anyToken (P.try $ P.string "===")
  preview <- manyTill anyToken (P.try $ P.string "\n\n_____\n\n")
  rest <- many anyToken
  let peviewM = markdown defaultMarkdownSettings (pack preview)
  let restM = markdown defaultMarkdownSettings (pack rest)
  return (title, peviewM, (peviewM <> restM))