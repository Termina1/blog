{-# LANGUAGE OverloadedStrings #-}
module Templates.Note
    (
      renderEntry,
      renderList,
      renderFull,
    ) where

import Models as M
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Templates.Icons (renderIcon, Icon(IconMore))
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.String (fromString)
import Data.Time.Format.Human (humanReadableTime)
import System.IO.Unsafe (unsafePerformIO)
import API (articleToLink)
import Templates.Fns (showLink)
import Data.Context (ContextEnv(senv))

renderEntry :: ContextEnv -> Note -> Html
renderEntry env note = H.article ! class_ "note-item" $ do
  H.h2 ! class_ "note-item__title" $ do
    H.a ! href (showLink (senv env) $ articleToLink note) $ toHtml $ M.title note
  H.time ! class_ "note-item__date" ! datetime (fromString $ iso8601Show $ created note) $ (fromString $ unsafePerformIO $ humanReadableTime $ created note)

  H.div ! class_ "note-item__preview" $ (preEscapedToMarkup $  M.previewText note)
  H.a ! class_ "note-item__more" ! href (showLink (senv env) $ articleToLink note) $ do
    H.span "Читать подробнее"
    H.span ! class_ "note-item__more-icon" $ (renderIcon IconMore)

renderList :: ContextEnv -> [Note] -> Html
renderList env notes = H.section ! class_ "notes-list" $ foldMap (renderEntry env) notes

renderFull :: ContextEnv -> Note -> Html
renderFull env note = H.article ! class_ "note-item note-item_full" $ do
  h1 ! class_ "note-item__title" $ (toHtml $ M.title note)
  H.time ! class_ "note-item__date" ! datetime (fromString $ iso8601Show $ created note) $ (fromString $ unsafePerformIO $ humanReadableTime $ created note)
  H.div ! class_ "note-item__preview" $ (toHtml $ M.fullText note)