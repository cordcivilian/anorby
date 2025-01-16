{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified System.Environment as Env
import qualified System.Random as Random

import qualified Control.Monad as Monad

import qualified Data.Ord as Ord
import qualified Data.List as List
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Pool as Pool

import qualified Text.Printf as Text

import qualified Database.SQLite.Simple as SQL

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
-- import Similarity
-- import Rank
import Marry

type Logger = T.Text -> IO ()

monolith :: Pool.Pool SQL.Connection -> Wai.Application
monolith pool = Mid.logStdout $ application TIO.putStrLn pool

application :: Logger -> Pool.Pool SQL.Connection -> Wai.Application
application _ pool request respond = do
  _ <- Wai.lazyRequestBody request
  let request_method = BS.unpack $ Wai.requestMethod request
      request_path = BS.unpack $ Wai.rawPathInfo request
  case (request_method, request_path) of
    ("GET", "/") ->
      Pool.withResource pool $ \conn ->
        respond =<< rootTemplateRoute conn request
    ("GET", "/whoami") -> undefined
    ("GET", "/whoami/ans") -> undefined
    ("GET", "/login") -> undefined
    ("GET", "/logout") -> undefined
    _ -> respond $ notFoundTemplateRoute request

-- ---------------------------------------------------------------------------

combineCSS :: [T.Text] -> T.Text
combineCSS = T.concat

cssEntry :: T.Text -> [T.Text] -> T.Text
cssEntry selector properties = T.unlines
  [ selector <> " {"
  , T.intercalate "\n" (map (\p -> "    " <> p <> ";") properties)
  , "}"
  ]

inlineCSSEntry :: [T.Text] -> T.Text
inlineCSSEntry properties = T.intercalate "; " properties

cssProperty :: T.Text -> T.Text -> T.Text
cssProperty property value = T.intercalate ": " [property, value]

fullCSS :: T.Text
fullCSS = combineCSS
  [ rootCSS
  , bodyHtmlCSS
  , underlineCSS
  , frameCSS
  , linkCSS
  , hrCSS
  , aorbsContainerCSS
  , aorbDisplayCSS
  , sorterCSS
  , sortByCSS
  , byDiceTargetCSS
  , byPolarTargetCSS
  , bySidedTargetCSS
  ]

rootCSS :: T.Text
rootCSS = cssEntry ":root"
  [ cssProperty "color-scheme" "light dark"
  ]

bodyHtmlCSS :: T.Text
bodyHtmlCSS = cssEntry "body, html"
  [ cssProperty "margin" "0 auto"
  , cssProperty "padding" "0 50px"
  , cssProperty "font-family" "'Lucida Console', monospace"
  , cssProperty "font-size" "20px"
  , cssProperty "max-width" "1280px"
  ]

underlineCSS :: T.Text
underlineCSS = cssEntry "span.underline"
  [ cssProperty "border-bottom" "6px solid black"
  , cssProperty "display" "inline-block"
  , cssProperty "line-height" "0.85"
  , cssProperty "padding" "0 1px"
  , cssProperty "margin" "0 1px -3px"
  ]

frameCSS :: T.Text
frameCSS = cssEntry ".frame"
  [ cssProperty "min-height" "100vh"
  , cssProperty "min-height" "100dvh"
  , cssProperty "text-align" "center"
  , cssProperty "align-content" "center"
  , cssProperty "border" "3px"
  , cssProperty "box-sizing" "border-box"
  ]

linkCSS :: T.Text
linkCSS = cssEntry "a"
  [ cssProperty "text-decoration" "none"
  , cssProperty "color" "#4169e1"
  ]

hrCSS :: T.Text
hrCSS = cssEntry "hr"
  [ cssProperty "border" "none"
  , cssProperty "height" "2px"
  , cssProperty "background-color" "lightgrey"
  , cssProperty "margin" "30px auto"
  ]

sorterCSS :: T.Text
sorterCSS = cssEntry "#sorter"
  [ cssProperty "display" "grid"
  , cssProperty "grid-template-columns" "1fr"
  , cssProperty "width" "fit-content"
  , cssProperty "margin" "0 auto"
  ]

sortByCSS :: T.Text
sortByCSS = cssEntry ".sort-by"
  [ cssProperty "padding" ".5rem"
  , cssProperty "text-align" "left"
  ]

byDiceTargetCSS :: T.Text
byDiceTargetCSS = cssEntry "#by-dice:target ~ #aorbs-container .aorb"
  [ cssProperty "order" "var(--order-dice) !important"
  ]

byPolarTargetCSS :: T.Text
byPolarTargetCSS = cssEntry "#by-polar:target ~ #aorbs-container .aorb"
  [ cssProperty "order" "var(--order-polar) !important"
  ]

bySidedTargetCSS :: T.Text
bySidedTargetCSS = cssEntry "#by-sided:target ~ #aorbs-container .aorb"
  [ cssProperty "order" "var(--order-sided) !important"
  ]

aorbsContainerCSS :: T.Text
aorbsContainerCSS = cssEntry "#aorbs-container"
  [ cssProperty "margin-top" "4rem"
  , cssProperty "display" "grid"
  , cssProperty "place-items" "center"
  , cssProperty "justify-content" "stretch"
  ]

aorbDisplayCSS :: T.Text
aorbDisplayCSS = combineCSS
  [ cssEntry ".aorb"
    [ cssProperty "border" "1px solid"
    , cssProperty "margin" "1rem"
    , cssProperty "max-width" "800px"
    , cssProperty "order" "var(--order-dice)"
    , cssProperty "padding" "1rem"
    , cssProperty "width" "100%"
    , cssProperty "text-align" "left"
    ]
  , cssEntry ".context"
    [ cssProperty "font-style" "italic"
    , cssProperty "color" "gray"
    , cssProperty "margin-bottom" "1rem"
    ]
  , cssEntry ".choice"
    [ cssProperty "margin" "0.5rem 0"
    ]
  , cssEntry ".choice.preferred"
    [ cssProperty "font-weight" "bold"
    , cssProperty "font-size" "1.1rem"
    ]
  , cssEntry ".choice.alternative"
    [ cssProperty "font-size" "0.9rem"
    , cssProperty "color" "gray"
    ]
  , cssEntry ".delta"
    [ cssProperty "color" "orange"
    , cssProperty "margin-left" "0.5rem"
    ]
  , cssEntry ".neutral"
    [ cssProperty "color" "gray"
    ]
  ]

-- ---------------------------------------------------------------------------

rootTemplateRoute :: SQL.Connection -> Wai.Request -> IO Wai.Response
rootTemplateRoute conn _ = do
  aorbs <- SQL.query_ conn "SELECT * FROM aorb" :: IO [Aorb]
  gen <- Random.getStdGen
  let (shuffledAorbs, _) = fisherYatesShuffle gen aorbs
  return $ Wai.responseLBS
    HTTP.status200
    [(Headers.hContentType, BS.pack "text/html")]
    (R.renderHtml $ rootTemplate shuffledAorbs)

rootTemplate :: [Aorb] -> H.Html
rootTemplate aorbs = H.docTypeHtml $ H.html $ do
  H.head $ do
    H.link H.! A.rel "icon" H.! A.href "data:,"
    H.meta H.! A.name "viewport" H.!
        A.content "width=device-width, initial-scale=1.0"
    H.title "anorby"
    H.style $ H.text fullCSS
  H.body $ do
    H.span H.! A.id "top" $ ""
    H.div H.! A.class_ "frame" $ do
      H.h1 $ do
        H.span H.! A.class_ "underline" $ "a"
        H.text "n"
        H.span H.! A.class_ "underline" $ "or"
        H.span H.! A.class_ "underline" $ "b"
        H.text "y"
      H.div $ do
        H.a H.! A.href "#baseline" $ "the underground census"
    H.span H.! A.id "baseline" $ ""
    H.div H.! A.class_ "frame" $ do
      H.h1 "baseline_100"
      H.h4 "what is this... blah blah blah"
      H.div H.! A.id "sorter" $ do
        H.div H.! A.class_ "sort-by" $ "sort by:"
        H.a H.! A.class_ "sort-by" H.! A.href "#by-sided" $ "> most one-sided"
        H.a H.! A.class_ "sort-by" H.! A.href "#by-polar" $ "> most polarizing"
        H.a H.! A.class_ "sort-by" H.! A.href "#by-dice" $ "> random"
    H.span H.! A.id "aorbs" $ ""
    H.div H.! A.class_ "frame" H.! A.style "padding-top: 20vh;" $ do
      H.div H.! A.id "by-sided" $ mempty
      H.div H.! A.id "by-dice" $ mempty
      H.div H.! A.id "by-polar" $ mempty
      H.div H.! A.id "aorbs-container" $ do
        let
          aorbDynamicCSS :: Int -> Int -> Int -> I.AttributeValue
          aorbDynamicCSS orderDice orderPolar orderSided =
            H.preEscapedTextValue $ inlineCSSEntry
              [ "--order-dice: " <> T.pack (show orderDice)
              , "--order-polar: " <> T.pack (show orderPolar)
              , "--order-sided: " <> T.pack (show orderSided)
              ]
          withOrders :: [(Int, (Aorb, Int, Int, Int))]
          withOrders = zip [(1::Int)..] $
            let byDice = aorbs
                byPolar = List.sortOn
                  (\a -> abs (aorbMean a - 0.5)) aorbs
                bySided = List.sortOn
                  (Ord.Down . \a -> abs (aorbMean a - 0.5)) aorbs
                lookupOrder list a = maybe 0 (+1) $ List.elemIndex a list
            in [ (a
                 , lookupOrder byDice a
                 , lookupOrder byPolar a
                 , lookupOrder bySided a)
                 | a <- aorbs ]
        Monad.forM_ withOrders $
          \(_, (aorb, orderDice, orderPolar, orderSided)) -> do
            H.div H.! A.class_ "aorb" H.!
              A.style (aorbDynamicCSS orderDice orderPolar orderSided) $ do
                H.div H.! A.class_ "context" $ H.toHtml (aorbCtx aorb)
                let mean = aorbMean aorb
                    delta = (abs (mean - 0.5)) * 100
                    formatDelta =
                        T.concat ["(+", T.pack (Text.printf "%.2f" delta), ")"]
                case compare mean 0.5 of
                  GT -> do -- b preferred
                    H.div H.! A.class_ "choice preferred" $ do
                      H.toHtml (aorbB aorb)
                      H.span H.! A.class_ "delta" $ H.toHtml formatDelta
                    H.div H.! A.class_ "choice alternative" $
                      H.toHtml (aorbA aorb)
                  LT -> do -- a preferred
                    H.div H.! A.class_ "choice preferred" $ do
                      H.toHtml (aorbA aorb)
                      H.span H.! A.class_ "delta" $ H.toHtml formatDelta
                    H.div H.! A.class_ "choice alternative" $
                      H.toHtml (aorbB aorb)
                  EQ -> do -- equal
                    H.div H.! A.class_ "choice" $ do
                      H.toHtml (aorbA aorb)
                      H.span H.! A.class_ "neutral" $ H.toHtml $ T.pack " (-)"
                    H.div H.! A.class_ "choice" $ do
                      H.toHtml (aorbB aorb)
                      H.span H.! A.class_ "neutral" $ H.toHtml $ T.pack " (-)"


notFoundTemplateRoute :: Wai.Request -> Wai.Response
notFoundTemplateRoute _ = Wai.responseLBS
  HTTP.status404
  [(Headers.hContentType, BS.pack "text/html")]
  (R.renderHtml notFoundTemplate)

notFoundTemplate :: H.Html
notFoundTemplate = H.docTypeHtml $ H.html $ do
  H.head $ do
    H.title "error"
    H.style $ I.preEscapedText fullCSS
  H.body $ do
    H.div H.! A.class_ "frame" $ do
      H.h1 "404 - not found"
      H.h1 $ do
        H.a H.! A.class_ "link" H.! A.href "/" $ "home"

-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  pool <- initPool ""
  maybePort <- Env.lookupEnv "PORT"
  let autoPort = 5001
      port = maybe autoPort read maybePort
  putStrLn $ "Server starting on port " ++ show (port :: Int)
  Warp.run port $ monolith pool
