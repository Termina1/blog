{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}

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
    absoluteURI
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

type IndexR = Get '[HTML] (RC.Context (Layout [Note]))
type NoteR = "notes" :> Capture "name" String :> Get '[HTML] (RC.Context (Layout Note))
type AboutR = "about" :> Get '[HTML] (RC.Context (Layout Note))
type RssR = "rss" :> Get '[RSS] (RC.Context [Note])

type API = IndexR :<|> NoteR :<|> AboutR :<|> RssR
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