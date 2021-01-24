module Renderers.Layout
  ( Layout(..)
  ) where

import Layout
import qualified Templates.HomeLayout as HomeL
import Renderers.Context (ToMarkupWithContext(..))

instance (ToMarkupWithContext a) => ToMarkupWithContext (Layout a) where
  toMarkupWithContext env (DefaultLayout entity) = HomeL.render env $ toMarkupWithContext env entity