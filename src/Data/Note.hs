module Data.Note
  (
    getNoteByName,
    listNotes,
    MonadNoteRemote(..),
    MonadNoteLocal(..)
  ) where

import Models (Note)
import Data.Config (MonadConfig(..))
import Data.NoteLocal (MonadNoteLocal(..))
import Data.NoteRemote (MonadNoteRemote(..))


getNoteByName :: (MonadNoteLocal m, MonadNoteRemote m, MonadConfig m) => String -> m (Either String (Maybe Note))
getNoteByName name = do
  localResult <- getNoteByNameLocal name
  case localResult of
    Left str -> return $ Left str
    Right mnote -> case mnote of
      Nothing -> do
        remoteResult <- getNoteByNameRemote name
        case remoteResult of
          Left err -> return $ Left err
          Right mnote -> case mnote of
            Nothing -> return $ Right Nothing
            Just art -> do
              time <- getTtl
              saveRemoteNote time art
              return $ Right mnote
      Just art -> return $ Right $ Just art

listNotes :: (MonadNoteLocal m, MonadNoteRemote m, MonadConfig m) => m (Either String [Note])
listNotes = do
  localResult <- listNoteLocal
  case localResult of
    Left err -> return $ Left err
    Right mnotes -> case mnotes of
      Just notes -> return $ Right notes
      Nothing -> do
        remoteResult <- listNotesRemote
        case remoteResult of
          Left err -> return $ Left err
          Right notes -> do
            time <- getTtl
            saveRemoteNoteList time notes
            return $ Right notes
