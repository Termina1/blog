{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Renderers.Context
  (
    ToMarkupWithContext(..),
  ) where

import Text.Blaze (ToMarkup(..), Markup)
import App (ServerEnv, AuthorEnv(..), BlogEnv(..))
import Data.Text (pack, Text)
import Data.Text.Lazy (toStrict)
import Models
import RSS
import Text.Atom.Feed (Feed(..), TextContent(..), Person(..), nullLink, Link(linkRel), Entry(..), nullEntry, EntryContent(..))
import API (aboutLink, rssLink, absoluteURI, homeLink, articleToLink)
import Data.Context
import Network.URI (URI)
import Data.List
import Data.Time.Format.ISO8601 (iso8601Show)
import Text.Blaze.Html.Renderer.Text (renderHtml)

showUri :: ContextEnv -> URI -> Text
showUri env uri = pack $ show $ absoluteURI (senv env) uri

class ToMarkupWithContext a where
  toMarkupWithContext :: ContextEnv -> a -> Markup

instance (ToMarkupWithContext a) => ToMarkup (Context a) where
  toMarkup (Context env ob) = toMarkupWithContext env ob

getNewestNote :: [Note] -> Note
getNewestNote notes = last $ sortOn created notes

toEntry :: ContextEnv -> Note -> Entry
toEntry env note =
  let entry = nullEntry (showUri env $ articleToLink note) (TextString (pack $ title note)) (pack $ iso8601Show $ created note) in
    entry {
      entryAuthors = getAuthors env,
      entryContent = Just (HTMLContent $ toStrict $ renderHtml $ fullText note)
    }

getAuthors :: ContextEnv -> [Person]
getAuthors env = [Person {
  personName = pack $ authorName (authorEnv env),
  personURI = Just $ showUri env aboutLink,
  personEmail = Just $ pack $ email (authorEnv env),
  personOther = []
}]

instance ToRSS (Context [Note]) where
  toRss (Context env notes) = Feed {
    feedId = showUri env rssLink,
    feedTitle = TextString (pack $ blogName (blogEnv env)),
    feedUpdated = pack $ iso8601Show $ created $ getNewestNote notes,
    feedCategories = [],
    feedContributors = [],
    feedGenerator = Nothing,
    feedIcon = Nothing,
    feedLinks = [(nullLink (showUri env homeLink)) { linkRel = Just (Left "self") }],
    feedLogo = Nothing,
    feedRights = Nothing,
    feedSubtitle = Nothing,
    feedAuthors = (getAuthors env),
    feedEntries = map (toEntry env) notes,
    feedAttrs = [],
    feedOther = []
  }
