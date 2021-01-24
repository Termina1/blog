module Server
  (
    WithAssets,
    API,
    app
  ) where

import Servant.Server.StaticFiles (serveDirectoryWebApp)
import Servant
import App (AppEnv, App, runApp)
import Models (Note)
import ServerHandlers (showAllNotes, showNote, showAboutMe, showAllNotesLayout)
import API (WithAssets, API, serverWithAssets)
import Renderers.Note

app :: AppEnv -> Application
app env = serve serverWithAssets (server env)

apiServer :: ServerT API App
apiServer = showAllNotesLayout
  :<|> showNote
  :<|> showAboutMe
  :<|> showAllNotes

server :: AppEnv -> Server WithAssets
server env = hoistServer
  serverWithAssets
  (runApp env)
  (apiServer :<|> serveDirectoryWebApp "./assets")