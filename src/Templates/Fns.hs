module Templates.Fns
  (
    showLink
  ) where

import Servant.Links (URI)
import App (ServerEnv(..))
import Text.Blaze (AttributeValue)
import Data.String
import API (absoluteURI)

showLink :: ServerEnv -> URI -> AttributeValue
showLink env uri = fromString $ show $ absoluteURI env uri