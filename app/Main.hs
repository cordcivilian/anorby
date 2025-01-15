{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified System.Environment as Env

import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import qualified Text.Blaze.Internal as I
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html.Renderer.Utf8 as R

import qualified Network.HTTP.Types as HTTP
import qualified Network.HTTP.Types.Header as Headers
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Middleware.RequestLogger as Mid

import Anorby
import Similarity
import Rank
import Marry

application :: Logger -> Wai.Application
application logger request respond = do
  body <- Wai.lazyRequestBody request
  let request_method = BS.unpack $ Wai.requestMethod request
      request_path = BS.unpack $ Wai.rawPathInfo request
  case (request_method, request_path) of
    ("GET", "/") -> undefined
    ("GET", "/whoami") -> undefined
    ("GET", "/login") -> undefined
    ("GET", "/logout") -> undefined
    _ -> respond $ notFoundTemplateRoute request

notFoundTemplateRoute :: Wai.Request -> Wai.Response
notFoundTemplateRoute _ = Wai.responseLBS
  HTTP.status404
  [(Headers.hContentType, BS.pack "text/html")]
  (R.renderHtml notFoundTemplate)

cssEntry :: T.Text -> [T.Text] -> T.Text
cssEntry selector properties = T.unlines
  [ selector <> " {"
  , T.intercalate "\n" (map (\p -> "    " <> p <> ";") properties)
  , "}"
  ]

cssProperty :: T.Text -> T.Text -> T.Text
cssProperty property value = T.intercalate ": " [property, value]

combineCSS :: [T.Text] -> T.Text
combineCSS = T.concat

rootCSS :: T.Text
rootCSS = cssEntry ":root" 
  [ cssProperty "color-scheme" "light dark"
  ]

fullCSS :: T.Text
fullCSS = combineCSS
  []

notFoundTemplate :: H.Html
notFoundTemplate = H.docTypeHtml $ H.html $ do
  H.head $ do
    H.title "error"
    H.style $ I.preEscapedText fullCSS
  H.body $ do
    H.div H.! A.id "frame" $ do
      H.h1 "404 - not found"
      H.h1 $ do
        H.a H.! A.class_ "link" H.! A.href "/" $ "home"

type Logger = T.Text -> IO ()
monolith :: Wai.Application
monolith = Mid.logStdout $ application TIO.putStrLn

main :: IO ()
main = do
  maybePort <- Env.lookupEnv "PORT"
  let autoPort = 5001
      port = maybe autoPort read maybePort
  putStrLn $ "Server starting on port " ++ show (port :: Int)
  Warp.run port $ monolith
