{-# LANGUAGE TemplateHaskell #-}

module Models
  ( Note(..)
  ) where

import Data.DateTime (DateTime)
import Text.Blaze.Html (Html)

data Note = Note
  { title :: String
  , previewText :: Html
  , name :: String
  , fullText  :: Html
  , created :: DateTime
  }
