{-# LANGUAGE RecordWildCards #-}

module Lib
    ( startApp
    , app
    ) where

import App (Env(..), AppEnv, ServerEnv(..), AuthorEnv(..), BlogEnv(..))
import Server (app)
import Network.Wai.Handler.Warp (run)


mkAppEnv :: IO AppEnv
mkAppEnv = do
    let notesFolder = "./notes"
    let ttl = 3600
    let senv = ServerEnv "localhost:8080/" "http:"
    let authorEnv = AuthorEnv "Viacheslav Shebanov" "terminal2010@gmail.com"
    let blogEnv = BlogEnv "Software Decay"
    pure Env{..}

startApp :: IO ()
startApp = do
  env <- mkAppEnv
  run 8080 (app env)