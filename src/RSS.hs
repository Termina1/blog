{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module RSS
  (
    ToRSS(..),
    RSS(..)
  ) where

import Data.Typeable
import Servant.API.ContentTypes
import qualified Network.HTTP.Media as M
import Text.Atom.Feed.Export (textFeed)
import Data.Text.Encoding (encodeUtf8)
import Data.ByteString.Lazy (fromStrict)
import Data.Maybe (fromMaybe)
import Data.Text.Lazy (empty, toStrict)
import Text.Atom.Feed (Feed)
import qualified Data.List.NonEmpty as NE

data RSS deriving Typeable

class ToRSS a where
  toRss :: a -> Feed

instance Accept RSS where
  contentTypes _ =
      "application" M.// "atom+xml" M./: ("charset", "utf-8") NE.:|
      [ "application" M.// "atom+xml" ]

instance (ToRSS a) => MimeRender RSS a where
    mimeRender _ = fromStrict . encodeUtf8 . toStrict . (fromMaybe empty) . textFeed . toRss