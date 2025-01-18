{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified System.Environment as Env
import qualified System.Random as Random

import qualified Control.Monad as Monad

import qualified Data.Binary.Builder as Builder
import qualified Data.Word as Word
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Digest.Pure.SHA as SHA
import qualified Data.List as List
import qualified Data.Ord as Ord
import qualified Data.Pool as Pool
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import qualified Data.Time.Clock.POSIX as POSIXTime
import qualified Data.Time.LocalTime as LocalTime
import qualified Data.Time.Calendar as Calendar
import qualified Data.Time.Clock as Clock
import qualified Data.Aeson as JSON
import qualified Data.Maybe as Maybe

import qualified Text.Printf as Text
import qualified Text.Read as Read

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
    ("GET", path)
      | Just uuid <- List.stripPrefix "/share/" path ->
        Pool.withResource pool $ \conn ->
          respond =<< sharedProfileTemplateRoute conn (T.pack uuid) request
    ("GET", "/") ->
      Pool.withResource pool $ \conn ->
        respond =<< rootTemplateRoute conn request
    ("GET", "/whoami") ->
      Pool.withResource pool $ \conn ->
        respond =<< profileTemplateRoute conn 1 request
    ("GET", "/ans") ->
      Pool.withResource pool $ \conn ->
        respond =<< ansTemplateRoute conn 1 request
    ("POST", "/ans/submit") -> do
      body <- Wai.strictRequestBody request
      let params = HTTP.parseQueryText $ BSL.toStrict body
          maybeAorbId = (Read.readMaybe . T.unpack)
            =<< Monad.join (lookup "aorb_id" params)
          maybeChoice = (Read.readMaybe . T.unpack)
            =<< Monad.join (lookup "choice" params)
          maybeToken = Monad.join (lookup "token" params)
      case (maybeAorbId, maybeChoice, maybeToken) of
        (Just aid, Just choice, Just token) ->
          Pool.withResource pool $ \conn ->
            respond =<< submitAnswerRoute
                        conn 1 aid (AorbAnswer choice) token request
        _ -> respond $ Wai.responseLBS
          HTTP.status400
          [(Headers.hContentType, BS.pack "text/html")]
          (R.renderHtml $ invalidSubmissionTemplate)
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

cssMediaQuery :: T.Text -> [T.Text] -> T.Text
cssMediaQuery query rules = T.unlines
  [ "@media " <> query <> " {"
  , T.unlines $ map ("  " <>) rules
  , "}"
  ]

fullCSS :: T.Text
fullCSS = combineCSS
  [ rootCSS
  , bodyHtmlCSS
  , underlineCSS
  , frameCSS
  , linkCSS
  , hrCSS
  , sorterCSS
  , sortByCSS
  , byDiceTargetCSS
  , byPolarTargetCSS
  , bySidedTargetCSS
  , byBasicTargetCSS
  , byFlakeTargetCSS
  , aorbsContainerCSS
  , aorbDisplayCSS
  , notchCSS
  , navBarCSS
  , ansPageCSS
  ]

rootCSS :: T.Text
rootCSS = cssEntry ":root"
  [ cssProperty "color-scheme" "light dark"
  ]

bodyHtmlCSS :: T.Text
bodyHtmlCSS = cssEntry "body, html"
  [ cssProperty "margin" "0 auto"
  , cssProperty "font-family" "'Lucida Console', monospace"
  , cssProperty "font-size" "20px"
  , cssProperty "max-width" "1280px"
  , cssProperty "width" "95vw"
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
  [ cssProperty "min-height" "100dvh"
  , cssProperty "text-align" "center"
  , cssProperty "align-content" "center"
  , cssProperty "border" "3px"
  , cssProperty "box-sizing" "border-box"
  , cssProperty "place-items" "center"
  , cssProperty "width" "100%"
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
  , cssProperty "width" "80vw"
  , cssProperty "max-width" "400px"
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

byBasicTargetCSS :: T.Text
byBasicTargetCSS = cssEntry "#by-basic:target ~ #aorbs-container .aorb"
  [ cssProperty "order" "var(--order-basic) !important"
  ]

byFlakeTargetCSS :: T.Text
byFlakeTargetCSS = cssEntry "#by-flake:target ~ #aorbs-container .aorb"
  [ cssProperty "order" "var(--order-flake) !important"
  ]

aorbsContainerCSS :: T.Text
aorbsContainerCSS = cssEntry "#aorbs-container, .aorbs-container"
  [ cssProperty "margin-top" "4rem"
  , cssProperty "display" "grid"
  , cssProperty "place-items" "center"
  , cssProperty "justify-content" "stretch"
  , cssProperty "width" "80vw"
  ]

aorbDisplayCSS :: T.Text
aorbDisplayCSS = combineCSS
  [ cssEntry ".aorb"
    [ cssProperty "border" "1px solid"
    , cssProperty "margin" "1rem"
    , cssProperty "max-width" "800px"
    , cssProperty "order" "var(--order-dice)"
    , cssProperty "padding" "1rem 1rem 0.8rem 1rem"
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
  , cssEntry ".choice.selected"
    [ cssProperty "color" "#4169e1"
    ]
  , cssEntry ".percentage"
    [ cssProperty "color" "#4169e1"
    , cssProperty "margin-left" "0.5rem"
    ]
  , cssEntry ".delta"
    [ cssProperty "color" "orange"
    , cssProperty "margin-left" "0.5rem"
    ]
  , cssEntry ".neutral"
    [ cssProperty "color" "gray"
    ]
  ]

notchCSS :: T.Text
notchCSS = cssEntry ".notch"
  [ cssProperty "position" "sticky"
  , cssProperty "top" "95dvh"
  , cssProperty "transform" "translateX(50%)"
  , cssProperty "z-index" "1000"
  , cssProperty "padding" "0.2rem"
  , cssProperty "border" "2px solid #4169e1"
  , cssProperty "background" "#4169e1"
  , cssProperty "border-radius" "10px"
  ]

sharedViewOverrides :: T.Text
sharedViewOverrides = combineCSS
  [ cssEntry ".percentage"
    [ cssProperty "color" "orange"
    ]
  , cssEntry ".delta"
    [ cssProperty "color" "orange"
    ]
  , cssEntry ".choice.selected"
    [ cssProperty "color" "orange"
    ]
  ]

ansPageCSS :: T.Text
ansPageCSS = T.unlines
  [ cssEntry ".ans-context"
    [ cssProperty "text-align" "center"
    , cssProperty "padding" "2rem 1rem"
    , cssProperty "font-style" "italic"
    , cssProperty "color" "gray"
    ]
  , cssEntry ".ans-choices"
    [ cssProperty "display" "grid"
    , cssProperty "grid-template-columns" "1fr"
    , cssProperty "grid-auto-rows" "1fr"
    , cssProperty "gap" "5rem"
    , cssProperty "padding" "1rem"
    , cssProperty "max-width" "1200px"
    , cssProperty "margin" "0 auto"
    , cssProperty "width" "80vw"
    ]
  , cssEntry ".ans-choice"
    [ cssProperty "border" "1px solid #ddd"
    , cssProperty "padding" "2rem"
    , cssProperty "border-radius" "0.5rem"
    , cssProperty "min-height" "160px"
    , cssProperty "text-align" "center"
    , cssProperty "cursor" "pointer"
    , cssProperty "outline" "2px solid #ddd"
    , cssProperty "outline-offset" "2px"
    , cssProperty "transition" "background-color 0.2s, outline-color 0.2s"
    ]
  , cssEntry ".ans-choice:hover"
    [ cssProperty "background-color" "#f5f5f5"
    , cssProperty "outline-color" "#aaa"
    ]
  , cssEntry "button.ans-choice"
    [ cssProperty "width" "100%"
    , cssProperty "height" "100%"
    , cssProperty "border" "none"
    , cssProperty "font" "inherit"
    ]
  , cssMediaQuery "(min-width: 1200px)"
    [ cssEntry ".ans-choices"
      [ cssProperty "grid-template-columns" "1fr 1fr"
      ]
    ]
  , cssMediaQuery "(prefers-color-scheme: dark)"
    [ cssEntry ".ans-choice"
      [ cssProperty "border-color" "#333"
      , cssProperty "outline-color" "#333"
      ]
    , cssEntry ".ans-choice:hover"
      [ cssProperty "background-color" "#2a2a2a"
      , cssProperty "outline-color" "#444"
      ]
    ]
  ]

cssMaxWidth :: Int -> [T.Text] -> T.Text
cssMaxWidth width = cssMediaQuery
  ("only screen and (max-width: " <> T.pack (show width) <> "px)")

-- ---------------------------------------------------------------------------

data NavLink = NavLink
  { linkPath :: T.Text
  , linkText :: T.Text
  , linkActive :: Bool
  }

navBarCSS :: T.Text
navBarCSS = T.unlines
  [ cssEntry ".nav-bar"
    [ cssProperty "display" "flex"
    , cssProperty "justify-content" "center"
    , cssProperty "gap" "0.75rem"
    , cssProperty "margin-bottom" "1rem"
    , cssProperty "flex-wrap" "wrap"
    , cssProperty "align-items" "center"
    ]
  , cssEntry ".nav-bar-row"
    [ cssProperty "display" "flex"
    , cssProperty "gap" "0.75rem"
    , cssProperty "align-items" "center"
    ]
  , cssEntry ".nav-separator"
    [ cssProperty "color" "gray"
    ]
  , cssEntry ".nav-link.active"
    [ cssProperty "color" "orange"
    ]
  , cssMaxWidth 350
    [ cssEntry ".nav-bar"
        [ cssProperty "flex-direction" "column"
        ]
    , cssEntry ".nav-bar-row"
        [ cssProperty "flex-direction" "column"
        ]
    , cssEntry ".nav-separator"
        [ cssProperty "display" "none"
        ]
    ]
  ]

navBar :: [NavLink] -> H.Html
navBar links = H.div H.! A.class_ "nav-bar" $ do
  let separator = H.span H.! A.class_ "nav-separator" $ "///"
      withSeparators [] = return ()
      withSeparators [x] = navLink x
      withSeparators (x:xs) = H.div H.! A.class_ "nav-bar-row" $ do
        navLink x
        separator
        withSeparators xs
      navLink link =
        if linkActive link
          then H.span H.! A.class_ "nav-link active" $
            H.toHtml $ linkText link
          else H.a H.! A.class_ "nav-link"
               H.! A.href (H.textValue $ linkPath link) $
            H.toHtml $ linkText link
  withSeparators links

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
      navBar [ NavLink "/" "home" True
             , NavLink "/whoami" "whoami" False
             , NavLink "/ans" "answer" False
             ]
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
    H.div H.! A.class_ "frame" H.! A.style "padding-top: 10vh;" $ do
      H.div H.! A.id "by-sided" $ mempty
      H.div H.! A.id "by-dice" $ mempty
      H.div H.! A.id "by-polar" $ mempty
      H.span H.! A.class_ "notch" $ do
        H.a H.! A.href "#baseline" $ "backtobasebasebase"
      aorbsComponent aorbs

aorbsComponent :: [Aorb] -> H.Html
aorbsComponent aorbs = do
  H.div H.! A.id "aorbs-container" $ do
    Monad.forM_ (aorbsWithOrders aorbs) $
      \(_, (aorb, orderDice, orderPolar, orderSided)) -> do
        H.div H.! A.class_ "aorb" H.!
          A.style (aorbDynamicCSS orderDice orderPolar orderSided) $ do
            H.div H.! A.class_ "context" $ H.toHtml (aorbCtx aorb)
            let mean = aorbMean aorb
                delta = (abs (mean - 0.5)) * 100
                formatDelta =
                    T.concat ["(+", T.pack (Text.printf "%.2f" delta), ")"]
            if delta < 0.01
              then do
                H.div H.! A.class_ "choice" $ do
                  H.toHtml (aorbA aorb)
                  H.span H.! A.class_ "neutral" $ H.toHtml $ T.pack " ...?"
                H.div H.! A.class_ "choice" $ do
                  H.toHtml (aorbB aorb)
                  H.span H.! A.class_ "neutral" $ H.toHtml $ T.pack " ...?"
              else if mean > 0.5
              then do
                H.div H.! A.class_ "choice preferred" $ do
                  H.toHtml (aorbB aorb)
                  H.span H.! A.class_ "delta" $ H.toHtml formatDelta
                H.div H.! A.class_ "choice alternative" $
                  H.toHtml (aorbA aorb)
              else do
                H.div H.! A.class_ "choice preferred" $ do
                  H.toHtml (aorbA aorb)
                  H.span H.! A.class_ "delta" $ H.toHtml formatDelta
                H.div H.! A.class_ "choice alternative" $
                  H.toHtml (aorbB aorb)
  where
    aorbsWithOrders :: [Aorb] -> [(Int, (Aorb, Int, Int, Int))]
    aorbsWithOrders as = zip [(1::Int)..] $
      let byDice = as
          byPolar = List.sortOn
            (\a -> abs (aorbMean a - 0.5)) as
          bySided = List.sortOn
            (Ord.Down . \a -> abs (aorbMean a - 0.5)) as
          lookupOrder list a = maybe 0 (+1) $ List.elemIndex a list
      in [ (a
           , lookupOrder byDice a
           , lookupOrder byPolar a
           , lookupOrder bySided a)
           | a <- as ]
    aorbDynamicCSS :: Int -> Int -> Int -> I.AttributeValue
    aorbDynamicCSS orderDice orderPolar orderSided =
      H.preEscapedTextValue $ inlineCSSEntry
        [ "--order-dice: " <> T.pack (show orderDice)
        , "--order-polar: " <> T.pack (show orderPolar)
        , "--order-sided: " <> T.pack (show orderSided)
        ]

profileTemplateRoute :: SQL.Connection -> UserID -> Wai.Request
                     -> IO Wai.Response
profileTemplateRoute conn uid _ = do
  userQ <- SQL.query conn "SELECT * FROM users WHERE id = ?" (SQL.Only uid)
  case userQ of
    (user:_) -> do
      answerCount <- getUserTotalAnswerCount conn uid
      if answerCount < 10
        then return $ Wai.responseLBS
          HTTP.status200
          [(Headers.hContentType, BS.pack "text/html")]
          (R.renderHtml $ profileTemplate [] Nothing Nothing Nothing True)
        else do
          myAorbs <- getUserAorbsFromControversialToCommonPlace conn uid
          let shareUrl =
                Just $
                "https://anorby.cordcivilian.com/share/" <> userUuid user
          return $ Wai.responseLBS
            HTTP.status200
            [(Headers.hContentType, BS.pack "text/html")]
            (R.renderHtml $
              profileTemplate myAorbs (userAorbId user) Nothing shareUrl False)
    [] -> return $ Wai.responseLBS
      HTTP.status404
      [(Headers.hContentType, BS.pack "text/html")]
      (R.renderHtml notFoundTemplate)

sharedProfileTemplateRoute :: SQL.Connection -> T.Text -> Wai.Request
                           -> IO Wai.Response
sharedProfileTemplateRoute conn uuid _ = do
  maybeUser <- getUserByUuid conn uuid
  case maybeUser of
    Just user -> do
      myAorbs <- getUserAorbsFromControversialToCommonPlace conn (userId user)
      return $ Wai.responseLBS
        HTTP.status200
        [(Headers.hContentType, BS.pack "text/html")]
        (R.renderHtml $
          profileTemplate myAorbs (userAorbId user) (Just uuid) Nothing False)
    Nothing -> return $ Wai.responseLBS
      HTTP.status404
      [(Headers.hContentType, BS.pack "text/html")]
      (R.renderHtml notFoundTemplate)

profileHead :: Maybe T.Text -> H.Html
profileHead maybeUuid = H.head $ do
  H.title $ case maybeUuid of
    Just uuid -> H.text $ "share/" <> uuid
    Nothing -> "whoami"
  H.link H.! A.rel "icon" H.! A.href "data:,"
  H.meta H.! A.name "viewport" H.!
    A.content "width=device-width, initial-scale=1.0"
  H.style $ I.preEscapedText $ case maybeUuid of
    Just _ -> fullCSS <> sharedViewOverrides
    Nothing -> fullCSS

profileHeadline :: Maybe T.Text -> H.Html -> H.Html
profileHeadline maybeUuid children = do
  H.span H.! A.id "top" $ ""
  H.div H.! A.class_ "frame" $ do
    navBar [ NavLink "/" "home" False
           , NavLink "/whoami" "whoami"
              (case maybeUuid of Just _ -> False ; _ -> True)
           , NavLink "/ans" "answer" False
           ]
    H.h1 $ case maybeUuid of
      Just uuid -> H.text $ "#" <> uuid
      Nothing -> "whoami"
    children

profileNotYetActive :: H.Html
profileNotYetActive = H.div $ do
  H.p "your profile will be activated after answering 10 questions"
  H.div $ do
    H.a H.! A.href "/ans" $ "begin"

profileMainAorb :: Maybe AorbID -> [AorbWithAnswer] -> H.Html
profileMainAorb mAid aorbs = case mAid of
  Just aid -> do
    H.span H.! A.id "main" $ ""
    H.div H.! A.class_ "frame" $ do
      H.h1 "main"
      H.div H.! A.class_ "aorbs-container" $ do
        mapM_ displayAorbWithAnswerLarge $
          filter (\awa -> aorbId (aorbData awa) == aid) aorbs
  Nothing -> mempty

profileCommonplaceAorbs :: [AorbWithAnswer] -> H.Html
profileCommonplaceAorbs aorbs = H.div H.! A.class_ "frame" $ do
  H.h1 "most commonplace"
  H.div H.! A.class_ "aorbs-container" $ do
    mapM_ displayAorbWithAnswerLarge (take 3 $ reverse aorbs)

profileControversialAorbs:: [AorbWithAnswer] -> H.Html
profileControversialAorbs aorbs = H.div H.! A.class_ "frame" $ do
  H.h1 "most controversial"
  H.div H.! A.class_ "aorbs-container" $ do
    mapM_ displayAorbWithAnswerLarge (take 3 aorbs)

profileAllAnswers :: [AorbWithAnswer] -> H.Html
profileAllAnswers aorbs = do
  H.span H.! A.id "all-answers" $ ""
  H.div H.! A.class_ "frame" $ do
    H.h1 "all answers"
    H.div H.! A.id "sorter" $ do
      H.div H.! A.class_ "sort-by" $ "sort by:"
      H.a H.! A.class_ "sort-by" H.!
        A.href "#by-basic" $ "> most commonplace"
      H.a H.! A.class_ "sort-by" H.!
        A.href "#by-flake" $ "> most controversial"
  H.div H.! A.class_ "frame" H.! A.style "padding-top: 10vh" $ do
    H.div H.! A.id "by-basic" $ mempty
    H.div H.! A.id "by-flake" $ mempty
    H.span H.! A.class_ "notch" $ do
      H.a H.! A.href "#all-answers" $ "backtoallanswersss"
    aorbsWithAnswersComponent aorbs

profileSharer :: Maybe T.Text -> Maybe T.Text -> H.Html
profileSharer maybeUuid shareUrl = case (maybeUuid, shareUrl) of
  (Nothing, Just url) -> H.div H.! A.class_ "frame" $ do
    H.h1 "share"
    H.div $ H.text url
  _ -> mempty

profileFullView :: Maybe AorbID -> [AorbWithAnswer]
                -> Maybe T.Text -> Maybe T.Text
                -> H.Html
profileFullView mAid aorbs maybeUuid shareUrl = do
  profileMainAorb mAid aorbs
  Monad.when (Maybe.isNothing mAid) $ H.span H.! A.id "main" $ ""
  profileCommonplaceAorbs aorbs
  profileControversialAorbs aorbs
  profileAllAnswers aorbs
  profileSharer maybeUuid shareUrl

profileTemplate :: [AorbWithAnswer] -> Maybe AorbID
                -> Maybe T.Text -> Maybe T.Text -> Bool
                -> H.Html
profileTemplate aorbs mAid maybeUuid shareUrl showMinimal =
  H.docTypeHtml $ H.html $ do
    profileHead maybeUuid
    H.body $ do
      profileHeadline maybeUuid $
        if showMinimal
          then profileNotYetActive
          else H.div $ do H.a H.! A.href "#main" $ "begin"
      if showMinimal
         then mempty
         else profileFullView mAid aorbs maybeUuid shareUrl

displayAorbWithAnswerLarge :: AorbWithAnswer -> H.Html
displayAorbWithAnswerLarge awa = H.div H.! A.class_ "aorb" $ do
  H.div H.! A.class_ "context" $ H.toHtml $ aorbCtx $ aorbData awa
  let aorb = aorbData awa
      ans = userAnswer awa
      percentage =
        case ans of
          AorbAnswer 0 -> 100 * (1 - aorbMean aorb)
          _ -> 100 * aorbMean aorb
  H.div H.! A.class_ (if ans == AorbAnswer 0
                      then "choice selected"
                      else "choice") $ do
      H.toHtml $ aorbA aorb
      Monad.when (ans == AorbAnswer 0) $
          H.span H.! A.class_ "percentage" $
              H.toHtml $ T.pack $ Text.printf " /\\/ %.0f%%" percentage
  H.div H.! A.class_ (if ans == AorbAnswer 1
                      then "choice selected"
                      else "choice") $ do
      H.toHtml $ aorbB aorb
      Monad.when (ans == AorbAnswer 1) $
          H.span H.! A.class_ "percentage" $
              H.toHtml $ T.pack $ Text.printf " /\\/ %.0f%%" percentage

aorbsWithAnswersComponent :: [AorbWithAnswer] -> H.Html
aorbsWithAnswersComponent aorbs = do
  H.div H.! A.id "aorbs-container" $ do
    Monad.forM_ (aorbsWithOrders aorbs) $
      \(_, (awa, orderBasic, orderFlake)) -> do
        H.div H.! A.class_ "aorb" H.!
          A.style (aorbDynamicCSS orderBasic orderFlake) $ do
            H.div H.! A.class_ "context" $ H.toHtml (aorbCtx $ aorbData awa)
            let aorb = aorbData awa
                ans = userAnswer awa
                percentage =
                  case ans of
                    AorbAnswer 0 -> 100 * (1 - aorbMean aorb)
                    _ -> 100 * aorbMean aorb
            H.div H.! A.class_ (if ans == AorbAnswer 0
                              then "choice selected"
                              else "choice") $ do
                H.toHtml $ aorbA aorb
                Monad.when (ans == AorbAnswer 0) $
                  H.span H.! A.class_ "percentage" $
                    H.toHtml $ T.pack $ Text.printf " /\\/ %.0f%%" percentage
            H.div H.! A.class_ (if ans == AorbAnswer 1
                              then "choice selected"
                              else "choice") $ do
                H.toHtml $ aorbB aorb
                Monad.when (ans == AorbAnswer 1) $
                  H.span H.! A.class_ "percentage" $
                    H.toHtml $ T.pack $ Text.printf " /\\/ %.0f%%" percentage
  where
    aorbsWithOrders :: [AorbWithAnswer]
                    -> [(Int, (AorbWithAnswer, Int, Int))]
    aorbsWithOrders awas = zip [(1::Int)..] $
      let byBasic = reverse awas
          byFlake = awas
          lookupOrder list a = maybe 0 (+1) $ List.elemIndex a list
      in [ (awa
         , lookupOrder byBasic awa
         , lookupOrder byFlake awa)
         | awa <- awas ]
    aorbDynamicCSS :: Int -> Int -> I.AttributeValue
    aorbDynamicCSS orderBasic orderFlake =
      H.preEscapedTextValue $ inlineCSSEntry
        [ "--order-basic: " <> T.pack (show orderBasic)
        , "--order-flake: " <> T.pack (show orderFlake)
        ]

submitAnswerRoute :: SQL.Connection -> UserID -> AorbID -> AorbAnswer -> T.Text
                  -> Wai.Request -> IO Wai.Response
submitAnswerRoute conn uid aid answer token _ = do
  answerCount <- getDailyAnswerCount conn uid
  if answerCount >= 10
    then do
      timeLeft <- getTimeUntilNextMidnight
      return $ Wai.responseLBS
        HTTP.status403
        [(Headers.hContentType, BS.pack "text/html")]
        (R.renderHtml $ dailyLimitTemplate timeLeft)
    else do
      isValid <- validateAnswerToken token uid aid
      if not isValid
        then return $ Wai.responseLBS
          HTTP.status403
          [(Headers.hContentType, BS.pack "text/html")]
          (R.renderHtml $ invalidTokenTemplate)
        else do
          existing <- SQL.query conn
            "SELECT 1 FROM aorb_answers WHERE user_id = ? AND aorb_id = ?"
            (uid, aid) :: IO [SQL.Only Int]
          case existing of
            (_:_) -> return $ Wai.responseLBS
              HTTP.status403
              [(Headers.hContentType, BS.pack "text/html")]
              (R.renderHtml $ alreadyAnsweredTemplate)
            [] -> do
              now <- POSIXTime.getCurrentTime
              let ans = AorbAnswers
                    { aorbUserId = uid
                    , aorbAorbId = aid
                    , aorbAnswer = answer
                    , aorbAnsweredOn = now
                    }
                  query = SQL.Query $ T.unwords
                    [ "INSERT INTO aorb_answers"
                    , "(user_id, aorb_id, answer, answered_on)"
                    , "VALUES (?, ?, ?, ?)"
                    ]
              SQL.execute conn query ans
              return $ Wai.responseLBS
                HTTP.status303
                [ (Headers.hLocation, BS.pack "/ans")
                , (Headers.hContentType, BS.pack "application/json")
                ]
                (JSON.encode $
                  JSON.object ["status" JSON..= ("success" :: T.Text)]
                )

ansTemplateRoute :: SQL.Connection -> UserID -> Wai.Request -> IO Wai.Response
ansTemplateRoute conn uid _ = do
  answerCount <- getDailyAnswerCount conn uid
  if answerCount >= 10
    then do
      timeLeft <- getTimeUntilNextMidnight
      return $ Wai.responseLBS
        HTTP.status200
        [(Headers.hContentType, BS.pack "text/html")]
        (R.renderHtml $ dailyLimitTemplate timeLeft)
    else do
      maybeNext <- getNextUnansweredAorb conn uid
      case maybeNext of
        Nothing -> return $ Wai.responseLBS
          HTTP.status200
          [(Headers.hContentType, BS.pack "text/html")]
          (R.renderHtml $ noMoreQuestionsTemplate)
        Just aorb -> do
          gen <- Random.getStdGen
          let (shouldSwap, _) = Random.random gen
          token <- generateAnswerToken uid (aorbId aorb)
          return $ Wai.responseLBS
            HTTP.status200
            [(Headers.hContentType, BS.pack "text/html")]
            (R.renderHtml $ ansTemplate aorb shouldSwap token)

ansTemplate :: Aorb -> Bool -> T.Text -> H.Html
ansTemplate aorb shouldSwap token = H.docTypeHtml $ H.html $ do
  H.head $ do
    H.title "answer"
    H.link H.! A.rel "icon" H.! A.href "data:,"
    H.meta H.! A.name "viewport" H.!
      A.content "width=device-width, initial-scale=1.0"
    H.style $ I.preEscapedText fullCSS
  H.body $ do
    H.div H.! A.class_ "frame" $ do
      navBar [ NavLink "/" "home" False
             , NavLink "/whoami" "whoami" False
             , NavLink "/ans" "answer" True
             ]
      H.div H.! A.class_ "ans-context" $
        H.toHtml (aorbCtx aorb)
      H.div H.! A.class_ "ans-choices" $ do
        if shouldSwap
          then do
            makeChoice aorb token (aorbB aorb) 1
            makeChoice aorb token (aorbA aorb) 0
          else do
            makeChoice aorb token (aorbA aorb) 0
            makeChoice aorb token (aorbB aorb) 1
  where
    makeChoice :: Aorb -> T.Text -> T.Text -> Word.Word8 -> H.Html
    makeChoice a t choice value = do
      H.form H.! A.method "POST" H.! A.action "/ans/submit" $ do
        H.input H.! A.type_ "hidden" H.!
          A.name "aorb_id" H.!
          A.value (H.toValue $ show $ aorbId a)
        H.input H.! A.type_ "hidden" H.!
          A.name "token" H.!
          A.value (H.textValue t)
        H.input H.! A.type_ "hidden" H.!
          A.name "choice" H.!
          A.value (H.toValue $ show value)
        H.button H.!
          A.type_ "submit" H.!
          A.class_ "ans-choice" $
          H.toHtml choice

data MessageTemplate = MessageTemplate
  { messageTitle :: T.Text
  , messageHeading :: T.Text
  , messageLink :: (T.Text, T.Text)
  }

msgTemplate :: MessageTemplate -> H.Html
msgTemplate template = H.docTypeHtml $ H.html $ do
  H.head $ do
    H.title $ H.toHtml $ messageTitle template
    H.link H.! A.rel "icon" H.! A.href "data:,"
    H.meta H.! A.name "viewport" H.!
      A.content "width=device-width, initial-scale=1.0"
    H.style $ I.preEscapedText fullCSS
  H.body $ do
    H.div H.! A.class_ "frame" $ do
      H.h1 $ H.toHtml $ messageHeading template
      H.div $ do
        H.a H.! A.href (H.textValue $ fst $ messageLink template) $
          H.toHtml $ snd $ messageLink template

dailyLimitTemplate :: T.Text -> H.Html
dailyLimitTemplate timeLeft = msgTemplate MessageTemplate
  { messageTitle = "daily limit"
  , messageHeading = "daily answer limit reached, come back in " <> timeLeft
  , messageLink = ("/whoami", "back to profile")
  }

noMoreQuestionsTemplate :: H.Html
noMoreQuestionsTemplate = msgTemplate MessageTemplate
  { messageTitle = "no more questions"
  , messageHeading = "no more questions"
  , messageLink = ("/whoami", "back to profile")
  }

alreadyAnsweredTemplate :: H.Html
alreadyAnsweredTemplate = msgTemplate MessageTemplate
  { messageTitle = "already answered"
  , messageHeading = "403 - question already answered"
  , messageLink = ("/ans", "next question")
  }

invalidTokenTemplate :: H.Html
invalidTokenTemplate = msgTemplate MessageTemplate
  { messageTitle = "invalid token"
  , messageHeading = "403 - invalid or expired token"
  , messageLink = ("/ans", "try again")
  }

invalidSubmissionTemplate :: H.Html
invalidSubmissionTemplate = msgTemplate MessageTemplate
  { messageTitle = "invalid submission"
  , messageHeading = "400 - invalid submission format"
  , messageLink = ("/ans", "try again")
  }

notFoundTemplate :: H.Html
notFoundTemplate = msgTemplate MessageTemplate
  { messageTitle = "error"
  , messageHeading = "404 - not found"
  , messageLink = ("/", "go home")
  }

notFoundTemplateRoute :: Wai.Request -> Wai.Response
notFoundTemplateRoute _ = Wai.responseLBS
  HTTP.status404
  [(Headers.hContentType, BS.pack "text/html")]
  (R.renderHtml notFoundTemplate)

-- ---------------------------------------------------------------------------

getTimeUntilNextMidnight :: IO T.Text
getTimeUntilNextMidnight = do
  now <- Clock.getCurrentTime
  let utcTime = LocalTime.utcToLocalTime LocalTime.utc now
      currentDay = LocalTime.localDay utcTime
      nextDay = Calendar.addDays 1 currentDay
      nextMidnight = LocalTime.LocalTime nextDay (LocalTime.TimeOfDay 0 0 0)
      nextMidnightUTC = LocalTime.localTimeToUTC LocalTime.utc nextMidnight
      diffSeconds = round $ Clock.diffUTCTime nextMidnightUTC now
      hours = div diffSeconds 3600
      minutes = div (rem diffSeconds  3600) 60
  return $ if (hours :: Int) > 0
    then T.pack $ show hours <> " hours"
    else T.pack $ show minutes <> " minutes"

generateAnswerToken :: UserID -> AorbID -> IO T.Text
generateAnswerToken uid aid = do
  maybeSecret <- Env.lookupEnv "ANORBY"
  now <- POSIXTime.getPOSIXTime
  let expiry = now + 300
      tokenData = BSL.concat
        [ Builder.toLazyByteString $ Builder.putWord64host $ fromIntegral uid
        , Builder.toLazyByteString $ Builder.putWord64host $ fromIntegral aid
        , Builder.toLazyByteString $ Builder.putWord64host $ truncate expiry
        ]
      secret = BSL.fromStrict $ TE.encodeUtf8 $
        T.pack $ maybe "no-secret" id maybeSecret
      signature = SHA.showDigest $ SHA.hmacSha256 secret tokenData
  return $ T.pack $
    show uid ++ "." ++
    show aid ++ "." ++
    show (floor expiry :: Integer) ++ "." ++
    signature

validateAnswerToken :: T.Text -> UserID -> AorbID -> IO Bool
validateAnswerToken token expectedUid expectedAid = do
  maybeSecret <- Env.lookupEnv "ANORBY"
  now <- POSIXTime.getPOSIXTime
  let parts = T.splitOn "." token
  case parts of
    [uidStr, aidStr, expiryStr, signature] -> do
      case
        ( Read.readMaybe (T.unpack uidStr) :: Maybe Int
        , Read.readMaybe (T.unpack aidStr) :: Maybe Int
        , Read.readMaybe (T.unpack expiryStr) :: Maybe Integer
        ) of
        (Just uid, Just aid, Just expiry) ->
          if fromIntegral uid /= expectedUid || fromIntegral aid /= expectedAid
            then return False
            else if fromIntegral expiry < now
              then return False
              else do
                let tokenData = BSL.concat
                      [ Builder.toLazyByteString $
                        Builder.putWord64host $ fromIntegral uid
                      , Builder.toLazyByteString $
                        Builder.putWord64host $ fromIntegral aid
                      , Builder.toLazyByteString $
                        Builder.putWord64host $ fromIntegral expiry
                      ]
                    secret = BSL.fromStrict $ TE.encodeUtf8 $
                      T.pack $ maybe "no-secret" id maybeSecret
                    expectedSignature =
                      SHA.showDigest $ SHA.hmacSha256 secret tokenData
                return $ signature == T.pack expectedSignature
        _ -> return False
    _ -> return False

-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  pool <- initPool "data/mock-anorby-20250118114756.db"
  maybePort <- Env.lookupEnv "PORT"
  let autoPort = 5001
      port = maybe autoPort read maybePort
  putStrLn $ "Server starting on port " ++ show (port :: Int)
  Warp.run port $ monolith pool
