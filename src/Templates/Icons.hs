{-# LANGUAGE OverloadedStrings #-}
module Templates.Icons
    (
      Icon(..),
      generateSprite,
      renderIcon,
    ) where

import Text.Blaze.Svg11 as S
import Text.Blaze.Svg11.Attributes as SA
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Data.String (fromString)


data Icon = IconMore deriving (Show, Enum, Bounded)

allIcons :: [Icon]
allIcons = enumFrom minBound

getViewBox :: Icon -> String
getViewBox IconMore = "0 0 92 92"
getViewBox _ = "0 0 32 32"

renderIcon :: Icon -> Markup
renderIcon icon = S.svg ! viewbox (fromString $ getViewBox icon) ! class_ "icon" $ do
  S.use ! xlinkHref (fromString ("#" ++ (show icon)))

renderIconPath :: Icon -> String
renderIconPath IconMore = "<path d=\"M37.426,46.086c-0.342,0.391-21.391,22.668-25.339,28.934c-0.811,1.288,0.854,3.089,2.16,2.162   c5.325-3.774,9.769-8.958,14.438-13.499c3.405-3.313,15.957-15.273,16.262-15.809c1.665-2.913-1.797-5.667-3.589-7.533   c-3.281-3.418-24.095-24.74-26.263-25.579c-0.811-0.439-1.672-0.483-2.454-0.249c-0.021,0.004-0.041,0.004-0.062,0.009   c0.007,0,0.014,0.002,0.022,0.004c-2.111,0.66-3.619,3.302-1.973,5.376C11.488,21.57,36.378,45.047,37.426,46.086z\"/><path d=\"M39.215,0.717c3.154,1.191,32.3,31.116,37.038,35.986c2.691,2.769,7.148,6.22,6.431,10.486   c-0.188,1.117-1.03,2.097-2.023,2.728c-0.367,0.375-34.475,34.777-42.558,40.507c-1.877,1.332-4.268-1.257-3.104-3.104   c5.551-8.81,35.755-40.814,36.414-41.531c-1.396-1.384-37.272-35.233-38.53-37.656c-2.339-2.993-0.142-6.791,2.905-7.709   L35.665,0.39C35.818,0.361,38.188,0.192,39.215,0.717z\"/>"


renderIconSVG :: Icon -> Markup
renderIconSVG icon = S.g ! id_ (fromString (show icon)) $ do
  preEscapedToMarkup $ renderIconPath icon

generateSprite :: Markup
generateSprite = H.div ! A.style "display: none" $ do
  docTypeSvg ! version "1.1" ! id_ "Layer_1" ! x "0px" ! y"0px" ! width "100px" ! height "100px" ! viewbox "0 0 32 32" ! enableBackground "new 0 0 32 32" ! xmlSpace "preserve" $ do
    foldMap renderIconSVG allIcons

