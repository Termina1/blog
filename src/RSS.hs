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
import Data.Text.Encoding (encodeUtf8)
import Data.ByteString.Lazy (fromStrict)
import Data.Text (pack)
import qualified Data.List.NonEmpty as NE
import qualified Text.RSS as R

data RSS deriving Typeable

class ToRSS a where
  toRss :: a -> R.RSS

instance Accept RSS where
  contentTypes _ =
      "application" M.// "rss+xml" M./: ("charset", "utf-8") NE.:|
      [ "application" M.// "rss+xml" ]

instance (ToRSS a) => MimeRender RSS a where
    mimeRender _ = fromStrict . encodeUtf8 . pack . R.showXML . R.rssToXML . toRss