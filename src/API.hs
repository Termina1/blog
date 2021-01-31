{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module API
  (
    WithAssets,
    API,
    IndexR,
    NoteR,
    RssR,
    serverWithAssets,
    articleToLink,
    homeLink,
    aboutLink,
    rssLink,
    absoluteURI,
    TJson
  ) where

import Servant
import Models (Note(..))
import Servant.HTML.Blaze (HTML)
import Layout
import Servant.Links (safeLink, URI(..), linkURI)
import App (ServerEnv(..))
import Data.Context as RC
import RSS
import Network.URI (URIAuth(..))
import Data.ByteString.Lazy (ByteString)
import Data.Aeson (Value)
import Data.ByteString.Lazy.Char8 (pack, unpack)
import qualified Network.HTTP.Media as M

data TJson

instance Accept TJson where
  contentType _ = "application" M.// "json"

instance MimeRender TJson String where
  mimeRender _ a = pack a

instance MimeUnrender TJson String where
  mimeUnrender _ a = Right $ unpack a

type IndexR = Get '[HTML] (RC.Context (Layout [Note]))
type NoteR = "notes" :> Capture "name" String :> Get '[HTML] (RC.Context (Layout Note))
type AboutR = "about" :> Get '[HTML] (RC.Context (Layout Note))
type RssR = "rss" :> Get '[RSS] (RC.Context [Note])
type UpdateR = "update" :> Header "X-Hub-Signature-256" String :> ReqBody '[TJson] String :> Post '[JSON] NoContent

type API = IndexR :<|> NoteR :<|> AboutR :<|> RssR :<|> UpdateR
type WithAssets = API :<|> ("assets" :> Raw)


serverWithAssets :: Proxy WithAssets
serverWithAssets = Proxy

articleToLink :: Note -> URI
articleToLink note = linkURI $ safeLink (Proxy :: Proxy API) (Proxy :: Proxy NoteR) (name note)

homeLink :: URI
homeLink = linkURI $ safeLink (Proxy :: Proxy API) (Proxy :: Proxy IndexR)

aboutLink :: URI
aboutLink = linkURI $ safeLink (Proxy :: Proxy API) (Proxy :: Proxy AboutR)

rssLink :: URI
rssLink = linkURI $ safeLink (Proxy :: Proxy API) (Proxy :: Proxy RssR)

absoluteURI :: ServerEnv -> URI -> URI
absoluteURI env uri = uri {
  uriScheme = (protocol env),
  uriAuthority = Just (URIAuth {
    uriUserInfo = "",
    uriRegName = domain env,
    uriPort = ""
  })
}