{-# LANGUAGE FlexibleInstances #-}

module Renderers.Note
  (
  ) where

import qualified Templates.Note as TNote
import Models (Note)
import Renderers.Context (ToMarkupWithContext(..))

instance ToMarkupWithContext Note where
  toMarkupWithContext env note = TNote.renderFull env note

instance ToMarkupWithContext [Note] where
  toMarkupWithContext env notes = TNote.renderList env notes