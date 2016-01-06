{-# LANGUAGE OverloadedStrings #-}
import Web.Scotty
import Text.Blaze.Html5.Attributes
import qualified Web.Scotty as S
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html.Renderer.Text as R
import Network.Wai.Middleware.Static
import Control.Concurrent.MVar
import Control.Monad.Trans
import Data.Monoid
import qualified Data.Text.Lazy as L
import qualified DataDefinition08 as Dat
blaze = S.html . R.renderHtml

render = do
  H.docType
  H.html $ do
    H.head $ do
      H.meta H.! charset "UTF-8"
      H.meta H.! A.httpEquiv "X-UA-Compatible" H.! A.content "IE=edge,chrome=1"
      H.title "SDG" 
      H.link H.! A.rel "stylesheet" H.! A.href "/css/styles.css" H.! A.type_ "text/css"
      H.script H.! A.type_ "text/javascript"  H.! A.src "scripts/scripts.js" $ "" 
    H.body $ do
       H.fieldset $ do
         H.legend $ H.h2 "= String Diagrams Generator ="
         H.section H.! class_ "content" $ do
           H.section H.! class_ "left" $ do
             H.form H.! A.method "post" H.! A.action "/postDiagram" $ do
               H.textarea "" H.! A.type_ "text" H.! A.name "content" H.! (A.placeholder "Copy your code here:") H.! A.cols "40" H.! A.rows "20" H.! A.style "resize: none;" H.! A.required ""
               H.br
               H.input H.! class_ "myButton" H.! A.type_ "submit" H.! A.value "Generate"
           H.section H.! class_ "right" $ do
             H.form H.! A.method "post" H.! A.action "/postDiagram" $ do
               H.section H.! class_ "slideshow" $ ""
diagrams :: String -> [String] -> H.Html
diagrams symbolN content = do
  H.docType
  H.html $ do
    H.head $ do
      H.meta H.! charset "UTF-8"
      H.meta H.! A.httpEquiv "X-UA-Compatible" H.! A.content "IE=edge,chrome=1"
      H.title "SDG"
      H.link H.! A.rel "stylesheet" H.! A.href "/css/styles.css" H.! A.type_ "text/css"
      H.script "" H.! A.src "scripts/scripts.js" H.! A.type_ "text/javascript" 
    H.body $ do
      H.fieldset $ do
        H.legend $ H.h2 "= String Diagrams Generator ="
        H.section H.! class_ "content" $ do
          H.section H.! class_ "left" $ do
            H.form H.! A.method "post" H.! A.action "/postDiagram" $ do
              H.textarea (H.toHtml symbolN) H.! A.type_ "text" H.! A.name "content" H.! A.cols "40" H.! A.rows "20" H.! A.style "resize: none;" H.! A.required ""
              H.br
              H.input H.! class_ "myButton" H.! A.type_ "submit" H.! A.value "Generate"
          H.section H.! class_ "right" $ do
            H.section H.! class_ "slideshow" $ do
              diagramsHtml content
            
diagramsHtml :: [String] -> H.Html
diagramsHtml messages = do
    mconcat $ map diagramHtml messages

-- Render message
diagramHtml :: String -> H.Html
diagramHtml content = do
    H.figure $ do
      H.img H.! A.src (H.toValue (concat ["/img/",content]))

main = do
  scotty 3000 $ do
    let contentMVar = newMVar " "
    get "/scripts/scripts.js" $ file "static/css/normalize.css"
    middleware $ staticPolicy (noDots >-> addBase "static")
    get "/" $ do
      blaze render
    post "/postDiagram" $ do
      ctnt <- (param "content") `rescue` (\msg -> return msg)
      let a = L.unpack ctnt
      liftIO $ Dat.bigTest a
      liftIO $ print a
      liftIO $ print $ Dat.dgNames a
      blaze (diagrams a (Dat.dgNames a) )
      
    notFound $ do
      text "404"


--http://adit.io/posts/2013-04-15-making-a-website-with-haskell.html
--
--
--
