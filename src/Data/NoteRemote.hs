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
import Data.Text (Text, pack)
import Data.Bifunctor (first)
import Text.MMark as MM (render, parse, MMark(..), MMarkErr)
import Lucid.Base (renderText)
import Text.Blaze.Html (Html, preEscapedToHtml)
import Text.Megaparsec as MP (ParseErrorBundle, Parsec, takeWhileP, try, parse, chunk, manyTill, anySingle, takeRest, fancyFailure, ErrorFancy(ErrorFail))
import Data.Set (singleton)

type Parser = Parsec String String

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
  (title, previewText, fullText) <- first show $ MP.parse noteParser name text
  status <- first show statusE
  let created = posixSecondsToUTCTime $ realToFrac $ modificationTime status
  return $ Just $ Note{..}

mmarkToHtml :: Either (ParseErrorBundle Text MMarkErr) MMark -> Parser Html
mmarkToHtml mmark = case mmark of
  Left err -> fancyFailure (singleton $ ErrorFail ("cannot parse markdown" ++ (show err)))
  Right doc -> return $ preEscapedToHtml $ renderText $ render doc

noteParser :: Parser (String, Html, Html)
noteParser = do
  title <- manyTill anySingle (MP.try $ chunk "===")
  preview <- manyTill anySingle (MP.try $ chunk "\n\n___\n\n")
  rest <- takeRest
  peviewM <- mmarkToHtml $ MM.parse "preview" (pack preview)
  restM <- mmarkToHtml $ MM.parse "rest" (pack rest)
  return (title, peviewM, (peviewM <> restM))