{-# LANGUAGE OverloadedStrings #-}
module Templates.HomeLayout
    (
      render
    ) where


import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Templates.Icons
import Data.Context (ContextEnv(..))
import API (homeLink, aboutLink, rssLink)
import Templates.Fns (showLink)
import App (BlogEnv(..))
import Data.String

render :: ContextEnv -> Markup -> Html
render env value = docTypeHtml $ do
  H.head $ do
    H.title $ fromString $ (blogName $ blogEnv env)
    H.link ! A.rel "stylesheet" ! href "https://cdn.jsdelivr.net/npm/modern-css-reset/dist/reset.min.css"
    H.link ! A.rel "stylesheet" ! href "https://fonts.googleapis.com/css2?family=Merriweather:wght@300;400;700&display=swap"
    H.link ! A.rel "stylesheet" ! href "/assets/style.css"

  body $ do
    H.div ! class_ "container" $ do
      generateSprite
      header $ do
        H.div $ do
          H.menu ! class_ "top-menu" $ do
            li ! class_ "top-menu__item" $ a ! href (showLink (senv env) aboutLink) $ "Обо мне"
            li ! class_ "top-menu__item" $ a ! href (showLink (senv env) homeLink) $ "Блог"
            li ! class_ "top-menu__item" $ a ! href (showLink (senv env) rssLink) $ "RSS"
        h1 $ a ! href (showLink (senv env) homeLink) $ fromString $ "<" ++ (blogName $ blogEnv env) ++ ">"
      H.div ! class_ "content" $ value