{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass #-}

module Lib
    ( startApp
    , app
    ) where

import App (Env(..), AppEnv, ServerEnv(..), AuthorEnv(..), BlogEnv(..))
import Server (app)
import Network.Wai.Handler.Warp (run)
import Data.Yaml (decodeFileThrow, FromJSON)
import GHC.Generics (Generic)


data InitConfig = InitConfig {
  serverDomain :: String,
  serverProtocol :: String
} deriving (Generic, FromJSON)

getConfig :: IO InitConfig
getConfig = decodeFileThrow "./config.yaml"

mkAppEnv :: IO AppEnv
mkAppEnv = do
    initConfig <- getConfig
    let notesFolder = "./notes"
    let ttl = 3600
    let senv = ServerEnv (serverDomain initConfig) (serverProtocol initConfig)
    let authorEnv = AuthorEnv "Viacheslav Shebanov" "terminal2010@gmail.com"
    let blogEnv = BlogEnv "Software Decay"
    pure Env{..}

startApp :: IO ()
startApp = do
  env <- mkAppEnv
  run 8080 (app env)