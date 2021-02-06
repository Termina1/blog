{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Renderers.Context
  (
    ToMarkupWithContext(..),
  ) where

import Text.Blaze (ToMarkup(..), Markup)
import App (ServerEnv, AuthorEnv(..), BlogEnv(..))
import Data.Text.Lazy (unpack)
import Models
import RSS
import Text.RSS (RSS(..), Item, ItemElem(..))
import API (aboutLink, rssLink, absoluteURI, homeLink, articleToLink)
import Data.Context
import Network.URI (URI)
import Data.List
import Data.Time.Format.ISO8601 (iso8601Show)
import Text.Blaze.Html.Renderer.Text (renderHtml)

class ToMarkupWithContext a where
  toMarkupWithContext :: ContextEnv -> a -> Markup

instance (ToMarkupWithContext a) => ToMarkup (Context a) where
  toMarkup (Context env ob) = toMarkupWithContext env ob

getNewestNote :: [Note] -> Note
getNewestNote notes = last $ sortOn created notes

toEntry :: ContextEnv -> Note -> Item
toEntry env note = [
    Title (title note),
    Link (absoluteURI (senv env) (articleToLink note)),
    Author (email (authorEnv env)),
    PubDate (created note),
    Description (unpack $ renderHtml $ fullText note)
  ]

instance ToRSS (Context [Note]) where
  toRss (Context env notes) = RSS (blogName (blogEnv env)) (absoluteURI (senv env) rssLink) "" [] (map (toEntry env) notes)
