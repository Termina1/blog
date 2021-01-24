module Data.NoteLocal
  (
    MonadNoteLocal(..)
  ) where

import Models (Note)

class (Monad m) => MonadNoteLocal m where
  getNoteByNameLocal :: String -> m (Either String (Maybe Note))
  listNoteLocal :: m (Either String (Maybe [Note]))
  saveRemoteNoteList :: Int -> [Note] -> m ()
  saveRemoteNote :: Int -> Note -> m ()