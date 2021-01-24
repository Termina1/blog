{-# LANGUAGE DeriveFunctor #-}
module Data.Context
  (
    Context(..),
    ContextEnv(..)
  ) where

import App (ServerEnv, AuthorEnv, BlogEnv)

data ContextEnv = ContextEnv {
  senv :: ServerEnv,
  authorEnv :: AuthorEnv,
  blogEnv :: BlogEnv
}

data Context a = Context ContextEnv a deriving (Functor)