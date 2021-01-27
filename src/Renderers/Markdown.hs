{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Renderers.Markdown
  ( createNote
  ) where

import Data.Text as T (Text, pack, take)
import Data.Bifunctor (first)
import Text.MMark as MM (render, parse, MMark(..), MMarkErr, useExtension)
import qualified Lucid.Base as L
import Text.Blaze.Html (Html, preEscapedToHtml)
import Text.Megaparsec as MP (ParseErrorBundle, Parsec, takeWhileP, try, parse, chunk, manyTill, anySingle, takeRest, fancyFailure, ErrorFancy(ErrorFail))
import Text.MMark.Extension (inlineRender, Inline, Extension, Inline(Link), asPlainText)
import qualified Data.List.NonEmpty as NL
import Lucid.Html5 (iframe_, width_, height_, src_, div_, class_)
import Data.Text.Internal.Search as S (indices)
import qualified Text.URI as U (URI, render, mkQueryKey, unRText)
import Text.URI.Lens
import qualified Text.URI.QQ as QQ
import Control.Lens
import Models (Note(..))
import System.Posix.Files (FileStatus, modificationTime)
import Data.Set (singleton)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)

type Parser = Parsec String String

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
  Right doc -> return $ preEscapedToHtml $ L.renderText $ render (useExtension youtubeExtenson doc)

noteParser :: Parser (String, Html, Html)
noteParser = do
  title <- manyTill anySingle (MP.try $ chunk "===")
  preview <- manyTill anySingle (MP.try $ chunk "\n\n___\n\n")
  rest <- takeRest
  peviewM <- mmarkToHtml $ MM.parse "preview" (pack preview)
  restM <- mmarkToHtml $ MM.parse "rest" (pack rest)
  return (title, peviewM, (peviewM <> restM))


youtubeRender :: (Inline -> L.Html ()) -> Inline -> L.Html ()
youtubeRender f i@(Link inner uri Nothing) = case S.indices "youtube.com" (U.render uri) of
  [] -> f i
  _ -> if (T.take 1 (asPlainText inner)) == "!" then (iframeRender uri) else f i
  where
    iframeRender :: U.URI -> L.Html ()
    iframeRender link = div_ [class_ "iframe-container"] (iframe_ [
        src_ (linkToEmbed link),
        (L.makeAttribute "frameborder" "0"),
        (L.makeAttribute "allow" "accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture"),
        (L.makeAttribute "allowfullscreen" "")
      ] "")

    linkToEmbed :: U.URI -> Text
    linkToEmbed link = let v = uri ^.. uriQuery . queryParam ([QQ.queryKey|v|]) in
      "https://www.youtube.com/embed/" <> (U.unRText $ head v)
youtubeRender f obj = f obj

youtubeExtenson :: Extension
youtubeExtenson = inlineRender youtubeRender
