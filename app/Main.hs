{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified System.Environment as Env
import qualified System.Random as Random
import qualified System.IO.Unsafe as Unsafe

import qualified Control.Monad as Monad
import qualified Control.Concurrent.MVar as MVar

import qualified Data.Binary.Builder as Builder
import qualified Data.Word as Word
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Digest.Pure.SHA as SHA
import qualified Data.List as List
import qualified Data.Ord as Ord
import qualified Data.Pool as Pool
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import qualified Data.Time.Clock.POSIX as POSIXTime
import qualified Data.Time.LocalTime as LocalTime
import qualified Data.Time.Calendar as Calendar
import qualified Data.Time.Clock as Clock
import qualified Data.Aeson as JSON
import qualified Data.Maybe as Maybe
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID

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
import qualified Network.Wai.Parse as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Middleware.RequestLogger as Mid
import qualified Network.Mail.Mime as Mail
import qualified Network.Mail.SMTP as SMTP
import qualified Web.Cookie as Cookie

import Config
import Cache
import Anorby
-- import Similarity
-- import Rank
import Marry
import Simulate

type Logger = T.Text -> IO ()

data AppState = AppState
  { appPool :: Pool.Pool SQL.Connection
  , appRootCache :: MVar.MVar (Cache.Cache [Aorb])
  }

monolith :: Pool.Pool SQL.Connection -> Wai.Application
monolith pool =
  let rootCache = Unsafe.unsafePerformIO $
        Cache.initCache (60 * Clock.secondsToNominalDiffTime 1)
      state = AppState
        { appPool = pool
        , appRootCache = rootCache
        }
  in Mid.logStdout $ application TIO.putStrLn state

application :: Logger -> AppState -> Wai.Application
application _ state request respond = do
  _ <- Wai.lazyRequestBody request

  let runHandlerWithConn :: (SQL.Connection -> Wai.Request -> IO Wai.Response)
                        -> IO Wai.ResponseReceived
      runHandlerWithConn handler =
        Pool.withResource pool (\conn -> handler conn request) >>= respond

      runProtectedHandlerWithConn ::
        (SQL.Connection -> UserID -> Wai.Request -> IO Wai.Response)
        -> IO Wai.ResponseReceived
      runProtectedHandlerWithConn handler =
        Pool.withResource pool (\conn -> do
          maybeUser <- getAuthenticatedUser conn request
          case maybeUser of
            Nothing -> return redirectToLogin
            Just user -> handler conn (userId user) request) >>= respond

      method = BS.unpack $ Wai.requestMethod request
      path = BS.unpack $ Wai.rawPathInfo request
      pool = appPool state

  case (method, path) of

    ("GET", "/") ->
      runHandlerWithConn (rootTemplateRoute state)

    ("GET", "/login") -> runHandlerWithConn loginGetRoute

    ("POST", "/login") -> runHandlerWithConn loginPostRoute

    ("GET", "/register") -> runHandlerWithConn registerGetRoute

    ("POST", "/register") -> runHandlerWithConn registerPostRoute

    ("GET", "/account") ->
      runProtectedHandlerWithConn accountTemplateRoute

    ("GET", "/logout") ->
      runProtectedHandlerWithConn logoutGetRoute

    ("GET", "/logout/confirm") ->
      runProtectedHandlerWithConn logoutConfirmRoute

    ("POST", "/logout/confirm") ->
      runProtectedHandlerWithConn logoutConfirmPostRoute

    ("GET", "/delete") ->
      runProtectedHandlerWithConn deleteGetRoute

    ("GET", "/delete/confirm") ->
      runProtectedHandlerWithConn deleteConfirmRoute

    ("POST", "/delete/confirm") ->
      runProtectedHandlerWithConn deleteConfirmPostRoute

    ("GET", p) | Just h <- List.stripPrefix "/auth/" p ->
      runHandlerWithConn (\conn -> authHashRoute conn (T.pack h))

    ("GET", "/whoami") ->
      runProtectedHandlerWithConn profileTemplateRoute

    ("GET", "/ans") ->
      runProtectedHandlerWithConn ansTemplateRoute

    ("GET", p) | Just uuid <- List.stripPrefix "/share/" p ->
      runHandlerWithConn (\conn ->
        sharedProfileTemplateRoute conn (T.pack uuid))

    ("GET", p) | Just aid <- readAorbId p ->
      runProtectedHandlerWithConn (\conn uid ->
        existingAnswerTemplateRoute conn uid aid)

    ("POST", "/ans/submit") -> do
      body <- Wai.strictRequestBody request
      case parseAnswerSubmission body of
        Just (aid, choice, token) ->
          runProtectedHandlerWithConn (\conn uid ->
            submitAnswerRoute conn uid aid choice token)
        Nothing -> respond invalidSubmissionResponse

    ("POST", "/ans/edit") -> do
      body <- Wai.strictRequestBody request
      case parseAnswerSubmission body of
        Just (aid, choice, token) ->
          runProtectedHandlerWithConn (\conn uid ->
            editAnswerRoute conn uid aid choice token)
        Nothing -> respond invalidSubmissionResponse

    ("POST", p) | Just aid <- readFavoriteAorbId p ->
      runProtectedHandlerWithConn (\conn uid ->
        setFavoriteAorbRoute conn uid aid)

    _ -> respond notFoundResponse

  where
    readAorbId :: String -> Maybe AorbID
    readAorbId path =
      Read.readMaybe =<< List.stripPrefix "/ans/" path

    readFavoriteAorbId :: String -> Maybe AorbID
    readFavoriteAorbId path =
      Read.readMaybe =<< List.stripPrefix "/aorb/favorite/" path

    parseAnswerSubmission :: BSL.ByteString
                              -> Maybe (AorbID, AorbAnswer, T.Text)
    parseAnswerSubmission body = do
      let params = HTTP.parseQueryText $ BSL.toStrict body
      reqAorbId <- (Read.readMaybe . T.unpack)
        =<< Monad.join (lookup "aorb_id" params)
      rawChoice <- (Read.readMaybe . T.unpack)
        =<< Monad.join (lookup "choice" params)
      token <- Monad.join (lookup "token" params)
      return (reqAorbId, AorbAnswer (rawChoice :: Word.Word8), token)

    invalidSubmissionResponse :: Wai.Response
    invalidSubmissionResponse = Wai.responseLBS
      HTTP.status400
      [(Headers.hContentType, BS.pack "text/html")]
      (R.renderHtml invalidSubmissionTemplate)

    notFoundResponse :: Wai.Response
    notFoundResponse = notFoundTemplateRoute request

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

baseCSS :: T.Text
baseCSS = combineCSS
  [ rootCSS
  , bodyHtmlCSS
  , frameCSS
  , linkCSS
  , hrCSS
  ]

rootPageCSS :: T.Text
rootPageCSS = combineCSS
  [ baseCSS
  , underlineCSS
  , navBarCSS
  , sorterCSS
  , sortByCSS
  , byDiceTargetCSS
  , byPolarTargetCSS
  , bySidedTargetCSS
  , aorbsContainerCSS
  , aorbDisplayCSS
  , notchCSS
  ]

profilePageCSS :: Maybe T.Text -> T.Text
profilePageCSS maybeUuid = combineCSS
  [ baseCSS
  , navBarCSS
  , sorterCSS
  , sortByCSS
  , byBasicTargetCSS
  , byFlakeTargetCSS
  , aorbsContainerCSS
  , aorbDisplayCSS
  , clickableAorbCSS
  , notchCSS
  , case maybeUuid of
      Just _ -> sharedViewOverrides
      Nothing -> ""
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
byBasicTargetCSS =
  cssEntry "#by-basic:target ~ #aorbs-container .aorb-clickable"
  [ cssProperty "order" "var(--order-basic) !important"
  ]

byFlakeTargetCSS :: T.Text
byFlakeTargetCSS =
  cssEntry "#by-flake:target ~ #aorbs-container .aorb-clickable"
  [ cssProperty "order" "var(--order-flake) !important"
  ]

aorbsContainerCSS :: T.Text
aorbsContainerCSS = cssEntry "#aorbs-container, .aorbs-container"
  [ cssProperty "margin-top" "4rem"
  , cssProperty "display" "grid"
  , cssProperty "place-items" "center"
  , cssProperty "justify-content" "stretch"
  , cssProperty "width" "80vw"
  , cssProperty "gap" "2rem"
  ]

aorbDisplayCSS :: T.Text
aorbDisplayCSS = combineCSS
  [ cssEntry ".aorb"
    [ cssProperty "border" "1px solid #ddd"
    , cssProperty "max-width" "800px"
    , cssProperty "order" "var(--order-dice)"
    , cssProperty "padding" "1rem 1rem 0.8rem 1rem"
    , cssProperty "width" "100%"
    , cssProperty "text-align" "left"
    , cssProperty "border-radius" "0.5rem"
    , cssProperty "outline" "2px solid #ddd"
    , cssProperty "outline-offset" "2px"
    , cssProperty "transition" "background-color 0.2s, outline-color 0.2s"
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

clickableAorbCSS :: T.Text
clickableAorbCSS = combineCSS
  [ cssEntry ".aorb-clickable"
    [ cssProperty "color" "inherit"
    , cssProperty "cursor" "pointer"
    , cssProperty "transition" "all 0.2s ease-in-out"
    , cssProperty "display" "block"
    , cssProperty "text-decoration" "none"
    , cssProperty "width" "95%"
    , cssProperty "place-items" "center"
    , cssProperty "order" "var(--order-flake)"
    ]
  , cssEntry ".aorb-clickable:hover"
    [ cssProperty "transform" "translateY(-2px)"
    , cssProperty "outline-color" "#aaa"
    ]
  , cssEntry ".aorb:hover"
    [ cssProperty "background-color" "#f5f5f5"
    ]
  , cssEntry ".aorb-favorite"
    [ cssProperty "border-color" "orange"
    , cssProperty "outline" "2px solid orange"
    , cssProperty "outline-offset" "2px"
    ]
  , cssEntry ".aorb-favorite:hover"
    [ cssProperty "background-color" "rgba(255, 165, 0, 0.05)"
    , cssProperty "outline-color" "orange"
    ]
  , cssEntry ".favorite-button"
    [ cssProperty "margin-top" "1rem"
    , cssProperty "padding" "0.5rem 1rem"
    , cssProperty "border" "2px solid #ddd"
    , cssProperty "background" "transparent"
    , cssProperty "cursor" "pointer"
    , cssProperty "font-family" "inherit"
    , cssProperty "font-size" "0.9rem"
    , cssProperty "transition" "all 0.2s"
    , cssProperty "display" "block"
    , cssProperty "width" "100%"
    ]
  , cssEntry ".favorite-button:hover"
    [ cssProperty "border-color" "orange"
    , cssProperty "color" "orange"
    ]
  , cssEntry ".favorite-button.active"
    [ cssProperty "border-color" "orange"
    , cssProperty "color" "orange"
    , cssProperty "background-color" "rgba(255, 165, 0, 0.1)"
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
ansPageCSS = combineCSS
  [ baseCSS
  , navBarCSS
  , ansComponentsCSS
  ]

ansComponentsCSS :: T.Text
ansComponentsCSS = combineCSS
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
  , cssEntry ".ans-choice.selected"
    [ cssProperty "border-color" "#4169e1"
    , cssProperty "outline-color" "#4169e1"
    , cssProperty "color" "#4169e1"
    , cssProperty "background-color" "rgba(65, 105, 225, 0.05)"
    ]
  , cssEntry ".ans-choice.selected:hover"
    [ cssProperty "background-color" "rgba(65, 105, 225, 0.1)"
    ]
  , cssEntry ".ans-choice.selected.favorite"
    [ cssProperty "border-color" "orange"
    , cssProperty "outline-color" "orange"
    , cssProperty "color" "orange"
    , cssProperty "background-color" "rgba(255, 165, 0, 0.05)"
    ]
  , cssEntry ".ans-choice.selected.favorite:hover"
    [ cssProperty "background-color" "rgba(255, 165, 0, 0.1)"
    ]
  , cssEntry "button.ans-choice"
    [ cssProperty "width" "100%"
    , cssProperty "height" "100%"
    , cssProperty "border" "none"
    , cssProperty "font" "inherit"
    ]
  , cssEntry ".favorite-section"
    [ cssProperty "margin-top" "3rem"
    , cssProperty "text-align" "center"
    ]
  , cssEntry ".favorite-button"
    [ cssProperty "padding" "1rem 2rem"
    , cssProperty "border" "2px solid #ddd"
    , cssProperty "background" "transparent"
    , cssProperty "cursor" "pointer"
    , cssProperty "font-family" "inherit"
    , cssProperty "font-size" "1rem"
    , cssProperty "transition" "all 0.2s"
    , cssProperty "border-radius" "0.5rem"
    ]
  , cssEntry ".favorite-button:hover"
    [ cssProperty "border-color" "orange"
    , cssProperty "color" "orange"
    ]
  , cssEntry ".favorite-button.active"
    [ cssProperty "border-color" "orange"
    , cssProperty "color" "orange"
    , cssProperty "background-color" "rgba(255, 165, 0, 0.1)"
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

authPageCSS :: T.Text
authPageCSS = combineCSS
  [ baseCSS
  , navBarCSS
  , cssEntry "div.auth-form"
    [ cssProperty "max-width" "400px"
    , cssProperty "margin" "0 auto"
    ]
  , cssEntry "form.auth-form"
    [ cssProperty "display" "flex"
    , cssProperty "flex-direction" "column"
    ]
  , cssEntry ".auth-input"
    [ cssProperty "padding" "0.5rem"
    , cssProperty "font-family" "inherit"
    , cssProperty "font-size" "inherit"
    ]
  , cssEntry ".auth-button"
    [ cssProperty "width" "100%"
    , cssProperty "padding" "0.5rem"
    , cssProperty "margin" "1rem 0"
    , cssProperty "font-family" "inherit"
    , cssProperty "font-size" "inherit"
    , cssProperty "cursor" "pointer"
    ]
  ]

accountPageCSS :: T.Text
accountPageCSS = combineCSS
  [ baseCSS
  , navBarCSS
  , cssEntry ".account-section"
    [ cssProperty "margin" "2rem auto"
    , cssProperty "max-width" "600px"
    , cssProperty "text-align" "left"
    ]
  , cssEntry ".account-heading"
    [ cssProperty "border-bottom" "2px solid #ddd"
    , cssProperty "padding-bottom" "0.5rem"
    , cssProperty "margin-bottom" "1rem"
    ]
  , cssEntry ".danger-zone"
    [ cssProperty "margin-top" "3rem"
    , cssProperty "padding" "1rem"
    , cssProperty "border" "2px solid #ff6b6b"
    , cssProperty "border-radius" "4px"
    ]
  , cssEntry ".danger-heading"
    [ cssProperty "color" "#ff6b6b"
    , cssProperty "margin-top" "0"
    ]
  , cssEntry ".confirm-button"
    [ cssProperty "background-color" "#ff6b6b"
    , cssProperty "color" "white"
    , cssProperty "border" "none"
    , cssProperty "padding" "0.5rem 1rem"
    , cssProperty "border-radius" "4px"
    , cssProperty "cursor" "pointer"
    , cssProperty "font-family" "inherit"
    , cssProperty "font-size" "1rem"
    , cssProperty "margin-top" "1rem"
    ]
  , cssEntry ".cancel-button"
    [ cssProperty "display" "block"
    , cssProperty "background-color" "transparent"
    , cssProperty "color" "inherit"
    , cssProperty "border" "1px solid #ddd"
    , cssProperty "padding" "0.5rem 1rem"
    , cssProperty "border-radius" "4px"
    , cssProperty "cursor" "pointer"
    , cssProperty "font-family" "inherit"
    , cssProperty "font-size" "1rem"
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

rootTemplateRoute :: AppState -> SQL.Connection -> Wai.Request
                  -> IO Wai.Response
rootTemplateRoute state conn _ = do
  cachedAorbs <- Cache.getFromCache "root_aorbs" (appRootCache state)
  aorbs <- case cachedAorbs of
    Just as -> return as
    Nothing -> do
      as <- SQL.query_ conn "SELECT * FROM aorb" :: IO [Aorb]
      gen <- Random.getStdGen
      let (shuffledAorbs, _) = fisherYatesShuffle gen as
      Cache.putInCache "root_aorbs" shuffledAorbs (appRootCache state)
      return shuffledAorbs
  return $ Wai.responseLBS
    HTTP.status200
    [(Headers.hContentType, BS.pack "text/html")]
    (R.renderHtml $ rootTemplate aorbs)


rootTemplate :: [Aorb] -> H.Html
rootTemplate aorbs = H.docTypeHtml $ H.html $ do
  H.head $ do
    H.link H.! A.rel "icon" H.! A.href "data:,"
    H.meta H.! A.name "viewport" H.!
        A.content "width=device-width, initial-scale=1.0"
    H.title "anorby"
    H.style $ H.text rootPageCSS
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
      publicAorbs aorbs

publicAorbs :: [Aorb] -> H.Html
publicAorbs aorbs = do
  H.div H.! A.id "aorbs-container" $ do
    Monad.forM_ (aorbWithOrders aorbs) $
      \(_, (aorb, orders)) -> do
        H.div H.! A.class_ "aorb" H.!
          A.style (aorbDynamicCSS (zip ["dice", "polar", "sided"] orders)) $ do
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

profileTemplateRoute :: SQL.Connection -> UserID -> Wai.Request
                     -> IO Wai.Response
profileTemplateRoute conn uid _ = do
  config <- getConfig
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
          let shareBaseUrl = if environment config == Production
                                then "https://anorby.cordcivilian.com/share/"
                                else "http://localhost:5001/share/"
              shareUrl = Just $ shareBaseUrl <> userUuid user
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
  H.style $ I.preEscapedText $ profilePageCSS maybeUuid

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
        mapM_ (\awa -> profileAorb awa mAid Nothing) $
          filter (\awa -> aorbId (aorbData awa) == aid) aorbs
  Nothing -> mempty

profileCommonplaceAorbs :: Maybe AorbID -> [AorbWithAnswer] -> H.Html
profileCommonplaceAorbs mAid aorbs = H.div H.! A.class_ "frame" $ do
  H.h1 "most commonplace"
  H.div H.! A.class_ "aorbs-container" $ do
    mapM_
      (\awa -> profileAorb awa mAid Nothing) (take 3 $ reverse aorbs)

profileControversialAorbs :: Maybe AorbID -> [AorbWithAnswer] -> H.Html
profileControversialAorbs mAid aorbs = H.div H.! A.class_ "frame" $ do
  H.h1 "most controversial"
  H.div H.! A.class_ "aorbs-container" $ do
    mapM_ (\awa -> profileAorb awa mAid Nothing) (take 3 aorbs)

profileAllAnswers :: Maybe AorbID -> [AorbWithAnswer] -> H.Html
profileAllAnswers mAid aorbs = do
  H.span H.! A.id "all-answers" $ ""
  H.div H.! A.class_ "frame" $ do
    H.h1 "all answers"
    H.div H.! A.id "sorter" $ do
      H.div H.! A.class_ "sort-by" $ "sort by:"
      H.a H.! A.class_ "sort-by" H.!
        A.href "#by-flake" $ "> most controversial"
      H.a H.! A.class_ "sort-by" H.!
        A.href "#by-basic" $ "> most commonplace"
  H.div H.! A.class_ "frame" H.! A.style "padding-top: 10vh" $ do
    H.div H.! A.id "by-basic" $ mempty
    H.div H.! A.id "by-flake" $ mempty
    H.span H.! A.class_ "notch" $ do
      H.a H.! A.href "#all-answers" $ "backtoallanswersss"
    profileOrdinaryAorbs mAid aorbs

profileSharer :: Maybe T.Text -> Maybe T.Text -> H.Html
profileSharer maybeUuid shareUrl = case (maybeUuid, shareUrl) of
  (Nothing, Just url) -> H.div H.! A.class_ "frame" $ do
    H.h1 "share"
    H.div $ H.text url
  _ -> mempty

accountManager :: Maybe T.Text -> H.Html
accountManager maybeUuid =
  case maybeUuid of
    Nothing ->
      H.div H.! A.class_ "frame" $ do
        H.div $ do H.a H.! A.href "/account" $ "manage my account"
    _ -> mempty

profileFullView :: Maybe AorbID -> [AorbWithAnswer]
                -> Maybe T.Text -> Maybe T.Text
                -> H.Html
profileFullView mAid aorbs maybeUuid shareUrl = do
  profileMainAorb mAid aorbs
  Monad.when (Maybe.isNothing mAid) $ H.span H.! A.id "main" $ ""
  profileCommonplaceAorbs mAid aorbs
  profileControversialAorbs mAid aorbs
  profileAllAnswers mAid aorbs
  profileSharer maybeUuid shareUrl
  accountManager maybeUuid

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

profileAorb :: AorbWithAnswer -> Maybe AorbID -> Maybe [Int] -> H.Html
profileAorb awa mFavoriteId mOrders = do
  let aorb = aorbData awa
      ans = userAnswer awa
      aid = aorbId aorb
      isFavorite = maybe False (== aid) mFavoriteId
      percentage =
        case ans of
          AorbAnswer 0 -> 100 * (1 - aorbMean aorb)
          _ -> 100 * aorbMean aorb
      favoriteClass = if isFavorite then " aorb-favorite" else ""
      dynamicStyle = case mOrders of
        Just orders ->
          (H.! A.style (aorbDynamicCSS (zip ["basic", "flake"] orders)))
        Nothing -> id
  dynamicStyle $ H.a H.! A.href (H.toValue $ "/ans/" ++ show aid)
    H.! A.class_ "aorb-clickable" $ do
      H.div H.! A.class_ (H.textValue $ "aorb" <> favoriteClass) $ do
        H.div H.! A.class_ "context" $ H.toHtml $ aorbCtx aorb
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

profileOrdinaryAorbs :: Maybe AorbID -> [AorbWithAnswer] -> H.Html
profileOrdinaryAorbs mAid aorbs = do
  H.div H.! A.id "aorbs-container" $ do
    Monad.forM_ (aorbWithAnswerWithOrders aorbs) $
      \(_, (awa, orders)) -> profileAorb awa mAid (Just orders)

type OrderingFunction a = [a] -> [a]
aorbsWithOrders :: (Eq a) => [a] -> [OrderingFunction a] -> [(Int, (a, [Int]))]
aorbsWithOrders as orderingFuncs = zip [(1::Int)..] $
  let orderedLists = map (\f -> f as) orderingFuncs
      lookupOrder list a = maybe 0 (+1) $ List.elemIndex a list
  in [ (a, map (\orderedList -> lookupOrder orderedList a) orderedLists)
     | a <- as ]

aorbWithOrders :: [Aorb] -> [(Int, (Aorb, [Int]))]
aorbWithOrders as = aorbsWithOrders as aorbOrderings

aorbWithAnswerWithOrders :: [AorbWithAnswer]
                         -> [(Int, (AorbWithAnswer, [Int]))]
aorbWithAnswerWithOrders awas = aorbsWithOrders awas aorbWithAnswerOrderings

aorbDynamicCSS :: [(String, Int)] -> H.AttributeValue
aorbDynamicCSS orderPairs =
  H.preEscapedTextValue $ inlineCSSEntry
    [ "--order-" <> T.pack name <> ": " <> T.pack (show order)
    | (name, order) <- orderPairs ]

aorbOrderings :: [OrderingFunction Aorb]
aorbOrderings =
  [ id  -- byDice
  , List.sortOn (\a -> abs (aorbMean a - 0.5))  -- byPolar
  , List.sortOn (Ord.Down . \a -> abs (aorbMean a - 0.5))  -- bySided
  ]

aorbWithAnswerOrderings :: [OrderingFunction AorbWithAnswer]
aorbWithAnswerOrderings =
  [ reverse  -- byBasic
  , id  -- byFlake
  ]

setFavoriteAorbRoute :: SQL.Connection -> UserID -> AorbID -> Wai.Request
                    -> IO Wai.Response
setFavoriteAorbRoute conn uid aid _ = do
  SQL.execute conn "UPDATE users SET aorb_id = ? WHERE id = ?" (aid, uid)
  return $ Wai.responseLBS
    HTTP.status303
    [ (Headers.hLocation, BS.pack $ "/ans/" ++ show aid)
    , (Headers.hContentType, BS.pack "text/html")
    ]
    ""

editAnswerRoute :: SQL.Connection -> UserID -> AorbID -> AorbAnswer -> T.Text
                -> Wai.Request -> IO Wai.Response
editAnswerRoute conn uid aid answer token _ = do
  isValid <- validateAnswerToken token uid aid
  if not isValid
    then return $ Wai.responseLBS
      HTTP.status403
      [(Headers.hContentType, BS.pack "text/html")]
      (R.renderHtml $ invalidTokenTemplate)
    else do
      now <- POSIXTime.getCurrentTime
      let ans = AorbAnswers
            { aorbUserId = uid
            , aorbAorbId = aid
            , aorbAnswer = answer
            , aorbAnsweredOn = now
            }
          query = SQL.Query $ T.unwords
            [ "UPDATE aorb_answers"
            , "SET answer = ?, answered_on = ?"
            , "WHERE user_id = ? AND aorb_id = ?"
            ]
      SQL.execute conn query
        (aorbAnswer ans, aorbAnsweredOn ans, aorbUserId ans, aorbAorbId ans)
      return $ Wai.responseLBS
        HTTP.status303
        [ (Headers.hLocation, BS.pack $ "/ans/" ++ show aid)
        , (Headers.hContentType, BS.pack "text/html")
        ]
        ""

existingAnswerTemplateRoute :: SQL.Connection -> UserID -> AorbID
                            -> Wai.Request
                            -> IO Wai.Response
existingAnswerTemplateRoute conn uid aid _ = do
  aorbResults <- SQL.query conn
    "SELECT * FROM aorb WHERE id = ?" (SQL.Only aid) :: IO [Aorb]
  case aorbResults of
    [aorb] -> do
      answerResults <- SQL.query conn
        "SELECT answer FROM aorb_answers WHERE user_id = ? AND aorb_id = ?"
        (uid, aid) :: IO [SQL.Only AorbAnswer]
      let currentAnswer = case answerResults of
            [SQL.Only ans] -> Just ans
            _ -> Nothing
      token <- generateAnswerToken uid aid
      userResults <- SQL.query conn
        "SELECT * FROM users WHERE id = ?" (SQL.Only uid) :: IO [User]
      let isFavorite = case userResults of
            [user] -> userAorbId user == Just aid
            _ -> False
      return $ Wai.responseLBS
        HTTP.status200
        [(Headers.hContentType, BS.pack "text/html")]
        (R.renderHtml $
          existingAnswerTemplate aorb currentAnswer isFavorite token)
    _ -> return $ notFoundTemplateRoute undefined

existingAnswerTemplate :: Aorb -> Maybe AorbAnswer -> Bool -> T.Text -> H.Html
existingAnswerTemplate aorb mCurrentAnswer isFavorite token =
  H.docTypeHtml $ H.html $ do
  H.head $ do
    H.title "answer"
    H.link H.! A.rel "icon" H.! A.href "data:,"
    H.meta H.! A.name "viewport" H.!
      A.content "width=device-width, initial-scale=1.0"
    H.style $ I.preEscapedText $ combineCSS
      [ baseCSS
      , navBarCSS
      , ansComponentsCSS
      , clickableAorbCSS
      ]
  H.body $ do
    H.div H.! A.class_ "frame" $ do
      navBar [ NavLink "/" "home" False
             , NavLink "/whoami" "whoami" False
             , NavLink "/ans" "answer" True
             ]
      H.div H.! A.class_ "ans-context" $
        H.toHtml (aorbCtx aorb)
      H.div H.! A.class_ "ans-choices" $ do
        makeExistingChoice aorb token (aorbA aorb) 0
          (mCurrentAnswer == Just (AorbAnswer 0)) isFavorite
        makeExistingChoice aorb token (aorbB aorb) 1
          (mCurrentAnswer == Just (AorbAnswer 1)) isFavorite
      if isFavorite
        then mempty
        else
          H.div H.! A.class_ "favorite-section" $ do
            H.form H.! A.method "POST"
                   H.! A.action
                     (H.toValue $ "/aorb/favorite/" ++ show (aorbId aorb)) $ do
              H.button
                H.! A.type_ "submit"
                H.! A.class_ (H.textValue $ "favorite-button active") $
                  "Set as Favorite Question"

makeExistingChoice :: Aorb -> T.Text -> T.Text -> Word.Word8 -> Bool -> Bool
                   -> H.Html
makeExistingChoice aorb token choice value isSelected isFavorite = do
  H.form H.! A.method "POST" H.! A.action "/ans/edit" $ do
    H.input H.! A.type_ "hidden" H.!
      A.name "aorb_id" H.!
      A.value (H.toValue $ show $ aorbId aorb)
    H.input H.! A.type_ "hidden" H.!
      A.name "token" H.!
      A.value (H.textValue token)
    H.input H.! A.type_ "hidden" H.!
      A.name "choice" H.!
      A.value (H.toValue $ show value)
    H.button H.!
      A.type_ "submit" H.!
      A.class_ (H.textValue $ "ans-choice" <>
                if isSelected
                then " selected" <> if isFavorite then " favorite" else ""
                else "") $
      H.toHtml choice

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
          let seed = aorbId aorb * 31 + uid
              gen = Random.mkStdGen seed
              (shouldSwap, _) = Random.random gen
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
    H.style $ I.preEscapedText ansPageCSS
  H.body $ do
    H.div H.! A.class_ "frame" $ do
      navBar [ NavLink "/" "home" False
             , NavLink "/whoami" "whoami" False
             , NavLink "/ans" "answer" True
             ]
      H.div H.! A.class_ "ans-context" $
        H.toHtml (aorbCtx aorb)
      H.div H.! A.class_ "ans-choices" $
        let (firstChoice, firstValue, secondChoice, secondValue) =
              if shouldSwap
                then (aorbB aorb, 1, aorbA aorb, 0)
                else (aorbA aorb, 0, aorbB aorb, 1)
        in do
          makeChoice aorb token firstChoice firstValue
          makeChoice aorb token secondChoice secondValue
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

redirectToLogin :: Wai.Response
redirectToLogin = Wai.responseLBS
  HTTP.status303
  [ (Headers.hLocation, "/login")
  , (Headers.hContentType, "text/html")
  ]
  ""

loginGetRoute :: SQL.Connection -> Wai.Request -> IO Wai.Response
loginGetRoute conn req = do
  maybeUser <- getAuthenticatedUser conn req
  case maybeUser of
    Just _ -> return $ Wai.responseLBS
      HTTP.status303
      [ (Headers.hLocation, "/whoami")
      , (Headers.hContentType, "text/html")
      ]
      ""
    Nothing -> do
      token <- generateAnswerToken (-1) (-1)
      return $ Wai.responseLBS
        HTTP.status200
        [(Headers.hContentType, "text/html")]
        (R.renderHtml $ loginTemplate token)

loginPostRoute :: SQL.Connection -> Wai.Request -> IO Wai.Response
loginPostRoute conn req = do
  (bodyParams, _) <- Wai.parseRequestBody Wai.lbsBackEnd req
  let emailParam = lookup "email" bodyParams
      tokenParam = lookup "token" bodyParams
  case (emailParam, tokenParam) of
    (Just email, Just token) -> do
      isValid <- validateAnswerToken (TE.decodeUtf8 token) (-1) (-1)
      if not isValid
        then return $ Wai.responseLBS
          HTTP.status403
          [(Headers.hContentType, "text/html")]
          (R.renderHtml invalidTokenTemplate)
        else do
          users <- SQL.query conn
            "SELECT * FROM users WHERE email = ?"
            (SQL.Only $ TE.decodeUtf8 email) :: IO [User]
          case users of
            [] -> return $ Wai.responseLBS
              HTTP.status404
              [(Headers.hContentType, "text/html")]
              (R.renderHtml $ msgTemplate MessageTemplate
                { messageTitle = "user not found"
                , messageHeading = "User not found"
                , messageLink = ("/register", "Register")
                })
            [user] -> do
              let query = SQL.Query $ T.unwords
                    [ "INSERT INTO auth"
                    , "(user_id, hash, created_on, last_accessed)"
                    , "VALUES (?, ?, ?, ?)"
                    ]
              hash <- generateAuthHash (TE.decodeUtf8 email)
              now <- POSIXTime.getCurrentTime
              SQL.execute conn query
                ( userId user
                , hash:: T.Text
                , show now :: String
                , show now :: String
                )
              emailConfig <- getEmailConfig
              sendAuthEmail emailConfig (TE.decodeUtf8 email) hash
              return $ Wai.responseLBS
                HTTP.status200
                [(Headers.hContentType, "text/html")]
                (R.renderHtml $ emailSentTemplate)
            _ -> return $ Wai.responseLBS
              HTTP.status500
              [(Headers.hContentType, "text/html")]
              (R.renderHtml notFoundTemplate)
    _ -> return $ Wai.responseLBS
      HTTP.status400
      [(Headers.hContentType, "text/html")]
      (R.renderHtml invalidSubmissionTemplate)

registerGetRoute :: SQL.Connection -> Wai.Request -> IO Wai.Response
registerGetRoute conn req = do
  maybeUser <- getAuthenticatedUser conn req
  case maybeUser of
    Just _ -> return $ Wai.responseLBS
      HTTP.status303
      [ (Headers.hLocation, "/whoami")
      , (Headers.hContentType, "text/html")
      ]
      ""
    Nothing -> do
      token <- generateAnswerToken (-1) (-1)
      return $ Wai.responseLBS
        HTTP.status200
        [(Headers.hContentType, "text/html")]
        (R.renderHtml $ registerTemplate token)

registerPostRoute :: SQL.Connection -> Wai.Request -> IO Wai.Response
registerPostRoute conn req = do
  (bodyParams, _) <- Wai.parseRequestBody Wai.lbsBackEnd req
  let emailParam = lookup "email" bodyParams
      tokenParam = lookup "token" bodyParams
  case (emailParam, tokenParam) of
    (Just email, Just token) -> do
      isValid <- validateAnswerToken (TE.decodeUtf8 token) (-1) (-1)
      if not isValid
        then return $ Wai.responseLBS
          HTTP.status403
          [(Headers.hContentType, "text/html")]
          (R.renderHtml invalidTokenTemplate)
        else do
          existing <- SQL.query conn
            "SELECT 1 FROM users WHERE email = ?"
            (SQL.Only $ TE.decodeUtf8 email) :: IO [SQL.Only Int]
          case existing of
            (_:_) -> return $ Wai.responseLBS
              HTTP.status409
              [(Headers.hContentType, "text/html")]
              (R.renderHtml $ msgTemplate MessageTemplate
                { messageTitle = "email exists"
                , messageHeading = "Email already registered"
                , messageLink = ("/login", "Login")
                })
            [] -> do
              now <- POSIXTime.getCurrentTime
              uuid <- UUID.toString <$> UUID.nextRandom
              let newUser = User
                    { userId = 0  -- will be set by sqlite
                    , userName = TE.decodeUtf8 email
                    , userEmail = TE.decodeUtf8 email
                    , userUuid = T.pack uuid
                    , userAorbId = Nothing
                    , userAssoc = Nothing
                    }
              SQL.execute conn
                "INSERT INTO users (name, email, uuid) VALUES (?, ?, ?)"
                (userName newUser, userEmail newUser, userUuid newUser)
              users <- SQL.query conn
                "SELECT * FROM users WHERE email = ?"
                (SQL.Only $ userEmail newUser) :: IO [User]
              case users of
                [user] -> do
                  let query = SQL.Query $ T.unwords
                        [ "INSERT INTO auth"
                        , "(user_id, hash, created_on, last_accessed)"
                        , "VALUES (?, ?, ?, ?)"
                        ]
                  hash <- generateAuthHash (userEmail newUser)
                  SQL.execute conn query
                    ( userId user
                    , hash :: T.Text
                    , show now :: String
                    , show now :: String
                    )
                  emailConfig <- getEmailConfig
                  sendAuthEmail emailConfig (userEmail newUser) hash
                  return $ Wai.responseLBS
                    HTTP.status200
                    [(Headers.hContentType, "text/html")]
                    (R.renderHtml $ emailSentTemplate)
                _ -> return $ Wai.responseLBS
                  HTTP.status500
                  [(Headers.hContentType, "text/html")]
                  (R.renderHtml notFoundTemplate)
    _ -> return $ Wai.responseLBS
      HTTP.status400
      [(Headers.hContentType, "text/html")]
      (R.renderHtml invalidSubmissionTemplate)

loginTemplate :: T.Text -> H.Html
loginTemplate token = H.docTypeHtml $ H.html $ do
  H.head $ do
    H.title "login"
    H.link H.! A.rel "icon" H.! A.href "data:,"
    H.meta H.! A.name "viewport" H.!
      A.content "width=device-width, initial-scale=1.0"
    H.style $ H.text authPageCSS
  H.body $ do
    H.div H.! A.class_ "frame" $ do
      navBar [ NavLink "/" "home" False
             , NavLink "/login" "login" True
             , NavLink "/register" "register" False
             ]
      H.div H.! A.class_ "auth-form" $ do
        H.form H.! A.class_ "auth-form"
          H.! A.method "POST" H.! A.action "/login" $ do
          H.input H.! A.type_ "email"
                 H.! A.name "email"
                 H.! A.placeholder "email"
                 H.! A.class_ "auth-input"
                 H.! A.required "required"
          H.input H.! A.type_ "hidden"
                 H.! A.name "token"
                 H.! A.value (H.textValue token)
          H.button H.! A.type_ "submit"
                  H.! A.class_ "auth-button" $ "login"

registerTemplate :: T.Text -> H.Html
registerTemplate token = H.docTypeHtml $ H.html $ do
  H.head $ do
    H.title "register"
    H.link H.! A.rel "icon" H.! A.href "data:,"
    H.meta H.! A.name "viewport" H.!
      A.content "width=device-width, initial-scale=1.0"
    H.style $ H.text authPageCSS
  H.body $ do
    H.div H.! A.class_ "frame" $ do
      navBar [ NavLink "/" "home" False
             , NavLink "/login" "login" False
             , NavLink "/register" "register" True
             ]
      H.div H.! A.class_ "auth-form" $ do
        H.form H.! A.class_ "auth-form"
          H.! A.method "POST" H.! A.action "/register" $ do
          H.input H.! A.type_ "email"
                 H.! A.name "email"
                 H.! A.placeholder "email"
                 H.! A.class_ "auth-input"
                 H.! A.required "required"
          H.input H.! A.type_ "hidden"
                 H.! A.name "token"
                 H.! A.value (H.textValue token)
          H.button H.! A.type_ "submit"
                  H.! A.class_ "auth-button" $ "register"

authHashRoute :: SQL.Connection -> T.Text -> Wai.Request -> IO Wai.Response
authHashRoute conn hash _ = do
  maybeUser <- getUserFromAuthHash conn hash
  case maybeUser of
    Nothing -> return $ Wai.responseLBS
      HTTP.status404
      [(Headers.hContentType, "text/html")]
      (R.renderHtml notFoundTemplate)
    Just _ -> do
      now <- POSIXTime.getCurrentTime
      SQL.execute conn
        "UPDATE auth SET last_accessed = ? WHERE hash = ?"
        (show now :: String, hash)
      return $ setCookie hash $ Wai.responseLBS
        HTTP.status303
        [ (Headers.hLocation, "/whoami")
        , (Headers.hContentType, "text/html")
        ]
        ""

accountTemplate :: User -> H.Html
accountTemplate user = H.docTypeHtml $ H.html $ do
  H.head $ do
    H.title "account"
    H.link H.! A.rel "icon" H.! A.href "data:,"
    H.meta H.! A.name "viewport" H.!
      A.content "width=device-width, initial-scale=1.0"
    H.style $ I.preEscapedText accountPageCSS
  H.body $ do
    H.div H.! A.class_ "frame" $ do
      navBar [ NavLink "/" "home" False
             , NavLink "/whoami" "whoami" False
             , NavLink "/ans" "answer" False
             ]
      H.div H.! A.class_ "account-section" $ do
        H.h2 H.! A.class_ "account-heading" $ "Account Information"
        H.p $ do
          H.text "Email: "
          H.toHtml $ userEmail user

        H.div H.! A.class_ "danger-zone" $ do
          H.h3 H.! A.class_ "danger-heading" $ "Danger Zone"
          H.p $ do
            H.text "logout from all devices: "
            H.a H.! A.href "/logout" $ "confirm via email"
          H.p $ do
            H.text "delete account and all data: "
            H.a H.! A.href "/delete" $ "confirm via email"

accountTemplateRoute :: SQL.Connection -> UserID -> Wai.Request
                    -> IO Wai.Response
accountTemplateRoute conn uid _ = do
  userQ <- SQL.query conn "SELECT * FROM users WHERE id = ?" (SQL.Only uid)
  case userQ of
    (user:_) -> do
      return $ Wai.responseLBS
        HTTP.status200
        [(Headers.hContentType, BS.pack "text/html")]
        (R.renderHtml $ accountTemplate user)
    [] -> return $ Wai.responseLBS
      HTTP.status404
      [(Headers.hContentType, BS.pack "text/html")]
      (R.renderHtml notFoundTemplate)

confirmTemplate :: T.Text -> T.Text -> T.Text -> T.Text -> T.Text -> T.Text
                -> H.Html
confirmTemplate title warning action token actionText cancelUrl =
  H.docTypeHtml $ H.html $ do
    H.head $ do
      H.title $ H.toHtml title
      H.link H.! A.rel "icon" H.! A.href "data:,"
      H.meta H.! A.name "viewport" H.!
        A.content "width=device-width, initial-scale=1.0"
      H.style $ I.preEscapedText accountPageCSS
    H.body $ do
      H.div H.! A.class_ "frame" $ do
        H.form H.! A.method "POST" H.! A.action (H.textValue action) $ do
          H.input H.! A.type_ "hidden"
                 H.! A.name "token"
                 H.! A.value (H.textValue token)
          H.button H.! A.type_ "submit" H.! A.class_ "confirm-button" $
            H.toHtml actionText
        H.h1 $ H.toHtml title
        H.p $ H.toHtml warning
        H.a H.! A.href (H.textValue cancelUrl) H.! A.class_ "cancel-button" $
          "Cancel"

logoutGetRoute :: SQL.Connection -> UserID -> Wai.Request -> IO Wai.Response
logoutGetRoute conn uid _ = do
  userQ <- SQL.query conn "SELECT * FROM users WHERE id = ?" (SQL.Only uid)
  case userQ of
    (user:_) -> do
      token <- generateAnswerToken uid (-1)
      emailConfig <- getEmailConfig
      sendLogoutConfirmEmail emailConfig (userEmail user) token
      return $ Wai.responseLBS
        HTTP.status200
        [(Headers.hContentType, BS.pack "text/html")]
        (R.renderHtml $ emailSentTemplate)
    [] -> return $ Wai.responseLBS
      HTTP.status404
      [(Headers.hContentType, BS.pack "text/html")]
      (R.renderHtml notFoundTemplate)

logoutConfirmRoute :: SQL.Connection -> UserID -> Wai.Request
                   -> IO Wai.Response
logoutConfirmRoute _ uid req = do
  let params = HTTP.parseQueryText $ Wai.rawQueryString req
  case Monad.join $ lookup "token" params of
    Just token -> do
      isValid <- validateAnswerToken token uid (-1)
      if isValid
        then return $ Wai.responseLBS
          HTTP.status200
          [(Headers.hContentType, BS.pack "text/html")]
          (R.renderHtml $ confirmTemplate
            "confirm logout"
            "are you sure you want to logout from all devices?"
            "/logout/confirm"
            token
            "logout"
            "/account")
        else return $ Wai.responseLBS
          HTTP.status403
          [(Headers.hContentType, BS.pack "text/html")]
          (R.renderHtml invalidTokenTemplate)
    Nothing -> return $ Wai.responseLBS
      HTTP.status400
      [(Headers.hContentType, BS.pack "text/html")]
      (R.renderHtml invalidSubmissionTemplate)

logoutConfirmPostRoute :: SQL.Connection -> UserID -> Wai.Request
                       -> IO Wai.Response
logoutConfirmPostRoute conn uid req = do
  (params, _) <- Wai.parseRequestBody Wai.lbsBackEnd req
  case lookup "token" params of
    Just tokenBS -> do
      let token = TE.decodeUtf8 tokenBS
      isValid <- validateAnswerToken token uid (-1)
      if isValid
        then do
          SQL.execute conn
            "DELETE FROM auth WHERE user_id = ?"
            (SQL.Only uid)
          return $ Wai.responseLBS
            HTTP.status303
            [ (Headers.hLocation, BS.pack "/")
            , (Headers.hContentType, BS.pack "text/html")
            , ("Set-Cookie",
               "X-Auth-Hash=; Path=/; Expires=Thu, 01 Jan 1970 00:00:00 GMT")
            ]
            ""
        else return $ Wai.responseLBS
          HTTP.status403
          [(Headers.hContentType, BS.pack "text/html")]
          (R.renderHtml invalidTokenTemplate)
    Nothing -> return $ Wai.responseLBS
      HTTP.status400
      [(Headers.hContentType, BS.pack "text/html")]
      (R.renderHtml invalidSubmissionTemplate)

deleteGetRoute :: SQL.Connection -> UserID -> Wai.Request -> IO Wai.Response
deleteGetRoute conn uid _ = do
  userQ <- SQL.query conn "SELECT * FROM users WHERE id = ?" (SQL.Only uid)
  case userQ of
    (user:_) -> do
      token <- generateAnswerToken uid (-1)
      emailConfig <- getEmailConfig
      sendDeleteConfirmEmail emailConfig (userEmail user) token
      return $ Wai.responseLBS
        HTTP.status200
        [(Headers.hContentType, BS.pack "text/html")]
        (R.renderHtml $ emailSentTemplate)
    [] -> return $ Wai.responseLBS
      HTTP.status404
      [(Headers.hContentType, BS.pack "text/html")]
      (R.renderHtml notFoundTemplate)

deleteConfirmRoute :: SQL.Connection -> UserID -> Wai.Request
                   -> IO Wai.Response
deleteConfirmRoute _ uid req = do
  let params = HTTP.parseQueryText $ Wai.rawQueryString req
  case Monad.join $ lookup "token" params of
    Just token -> do
      isValid <- validateAnswerToken token uid (-1)
      if isValid
        then return $ Wai.responseLBS
          HTTP.status200
          [(Headers.hContentType, BS.pack "text/html")]
          (R.renderHtml $ confirmTemplate
            "confirm account deletion"
            (  "warning: this action cannot be undone. "
            <> "all your data will be permanently deleted."
            )
            "/delete/confirm"
            token
            "delete account"
            "/account")
        else return $ Wai.responseLBS
          HTTP.status403
          [(Headers.hContentType, BS.pack "text/html")]
          (R.renderHtml invalidTokenTemplate)
    Nothing -> return $ Wai.responseLBS
      HTTP.status400
      [(Headers.hContentType, BS.pack "text/html")]
      (R.renderHtml invalidSubmissionTemplate)

deleteConfirmPostRoute :: SQL.Connection -> UserID -> Wai.Request
                       -> IO Wai.Response
deleteConfirmPostRoute conn uid req = do
  (params, _) <- Wai.parseRequestBody Wai.lbsBackEnd req
  case lookup "token" params of
    Just tokenBS -> do
      let token = TE.decodeUtf8 tokenBS
      isValid <- validateAnswerToken token uid (-1)
      if isValid
        then do
          SQL.withTransaction conn $ do
            SQL.execute conn
              "DELETE FROM aorb_answers WHERE user_id = ?"
              (SQL.Only uid)
            SQL.execute conn
              "DELETE FROM auth WHERE user_id = ?"
              (SQL.Only uid)
            SQL.execute conn
              "DELETE FROM users WHERE id = ?"
              (SQL.Only uid)
          return $ Wai.responseLBS
            HTTP.status303
            [ (Headers.hLocation, BS.pack "/")
            , (Headers.hContentType, BS.pack "text/html")
            , ("Set-Cookie",
               "X-Auth-Hash=; Path=/; Expires=Thu, 01 Jan 1970 00:00:00 GMT")
            ]
            ""
        else return $ Wai.responseLBS
          HTTP.status403
          [(Headers.hContentType, BS.pack "text/html")]
          (R.renderHtml invalidTokenTemplate)
    Nothing -> return $ Wai.responseLBS
      HTTP.status400
      [(Headers.hContentType, BS.pack "text/html")]
      (R.renderHtml invalidSubmissionTemplate)

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
    H.style $ I.preEscapedText baseCSS
  H.body $ do
    H.div H.! A.class_ "frame" $ do
      H.h1 $ H.toHtml $ messageHeading template
      H.div $ do
        H.a H.! A.href (H.textValue $ fst $ messageLink template) $
          H.toHtml $ snd $ messageLink template

emailSentTemplate :: H.Html
emailSentTemplate = msgTemplate MessageTemplate
  { messageTitle = "check your email"
  , messageHeading = "check your email"
  , messageLink = ("/", "home")
  }

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

getAuthenticatedUser :: SQL.Connection -> Wai.Request -> IO (Maybe User)
getAuthenticatedUser conn req = do
  case getCookie req of
    Nothing -> return Nothing
    Just cookieBS -> do
      let hash = TE.decodeUtf8 cookieBS
      getUserFromAuthHash conn hash

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
      minutes = div (rem diffSeconds 3600) 60
      roundedHours = if minutes > 30 then hours + 1 else hours :: Int
  return $ if roundedHours > 0
    then T.pack $ show roundedHours <> " hours"
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

getCookie :: Wai.Request -> Maybe BS.ByteString
getCookie req =
  case lookup "Cookie" (Wai.requestHeaders req) of
    Nothing -> Nothing
    Just cookies ->
      let parsedCookies = Cookie.parseCookies cookies
      in lookup "X-Auth-Hash" parsedCookies

setCookie :: T.Text -> Wai.Response -> Wai.Response
setCookie hash resp =
  let cookie = Cookie.defaultSetCookie
        { Cookie.setCookieName = "X-Auth-Hash"
        , Cookie.setCookieValue = TE.encodeUtf8 hash
        , Cookie.setCookiePath = Just "/"
        , Cookie.setCookieSecure = True
        , Cookie.setCookieSameSite = Just Cookie.sameSiteStrict
        , Cookie.setCookieHttpOnly = True
        }
      cookieHeader = ("Set-Cookie", BSL.toStrict $
                      Builder.toLazyByteString $
                      Cookie.renderSetCookie cookie)
  in Wai.mapResponseHeaders ((:) cookieHeader) resp

generateAuthHash :: T.Text -> IO T.Text
generateAuthHash email = do
  maybeSecret <- Env.lookupEnv "ANORBY"
  now <- POSIXTime.getPOSIXTime
  nonce <- B.pack <$>
    Monad.replicateM 32 (Random.randomRIO (0 :: Word.Word8, 255))
  let secret = BSL.fromStrict $ TE.encodeUtf8 $
                T.pack $ maybe "no-secret" id maybeSecret
      hashData = BSL.concat
        [ secret
        , BSL.fromStrict nonce
        , BSL.fromStrict $ TE.encodeUtf8 email
        , Builder.toLazyByteString $
            Builder.putWord64host $ truncate now
        ]
      hash1 = SHA.sha512 hashData
      hash2 = SHA.hmacSha512 secret
        (BSL.fromStrict $ BS.pack $ SHA.showDigest hash1)
  return $ T.pack $ SHA.showDigest hash2

-- ---------------------------------------------------------------------------

data EmailConfig = EmailConfig
  { emailHost :: String
  , emailPort :: Int
  , emailUser :: String
  , emailPass :: String
  , emailFrom :: T.Text
  , emailSend :: Bool
  , emailBaseUrl :: T.Text
  }

getEmailConfig :: IO EmailConfig
getEmailConfig = do
  config <- getConfig
  pass <- maybe "" id <$> Env.lookupEnv "SMTP"
  return $ EmailConfig
    { emailHost = "smtp.protonmail.ch"
    , emailPort = 587
    , emailUser = "anorby@cordcivilian.com"
    , emailPass = pass
    , emailFrom = "anorby@cordcivilian.com"
    , emailSend = environment config == Production
    , emailBaseUrl = if environment config == Production
                     then "https://anorby.cordcivilian.com"
                     else "http://localhost:5001"
    }

sendAuthEmail :: EmailConfig -> T.Text -> T.Text -> IO ()
sendAuthEmail config toEmail hash = do
  let subject = "Your Anorby Authentication Link"
      authUrl = emailBaseUrl config <> "/auth/" <> hash
      textBody = TL.fromStrict $
                 "Click this link to authenticate: " <> authUrl
      htmlBody = TL.fromStrict $
                 "<p>Click this link to authenticate: \
                 \<a href=\"" <> authUrl <> "\">" <> authUrl <> "</a></p>"

      from = Mail.Address (Just "anorby") (emailFrom config)
      to = Mail.Address Nothing toEmail

  if emailSend config
    then do
      mail <- Mail.simpleMail from to "" "" subject
        [(TL.toStrict textBody, "plain"), (TL.toStrict htmlBody, "html")]
      SMTP.sendMailWithLogin
        (emailHost config)
        (emailUser config)
        (emailPass config)
        mail
    else do
      putStrLn $
        "auth url for "
        ++ T.unpack toEmail
        ++ ": "
        ++ T.unpack authUrl

sendLogoutConfirmEmail :: EmailConfig -> T.Text -> T.Text -> IO ()
sendLogoutConfirmEmail config toEmail token = do
  let subject = "Confirm Anorby Logout"
      confirmUrl = emailBaseUrl config <> "/logout/confirm?token=" <> token
      textBody = TL.fromStrict $
                 "Click this link to confirm logout from all devices: "
                 <> confirmUrl
      htmlBody = TL.fromStrict $
                 "<p>Click this link to confirm logout from all devices: \
                 \<a href=\"" <> confirmUrl <> "\">"
                 <> confirmUrl <> "</a></p>"

      from = Mail.Address (Just "anorby") (emailFrom config)
      to = Mail.Address Nothing toEmail

  if emailSend config
    then do
      mail <- Mail.simpleMail from to "" "" subject
        [(TL.toStrict textBody, "plain"), (TL.toStrict htmlBody, "html")]
      SMTP.sendMailWithLogin
        (emailHost config)
        (emailUser config)
        (emailPass config)
        mail
    else do
      putStrLn $
        "logout confirm url for "
        ++ T.unpack toEmail
        ++ ": "
        ++ T.unpack confirmUrl

sendDeleteConfirmEmail :: EmailConfig -> T.Text -> T.Text -> IO ()
sendDeleteConfirmEmail config toEmail token = do
  let subject = "Confirm Anorby Account Deletion"
      confirmUrl = emailBaseUrl config <> "/delete/confirm?token=" <> token
      textBody = TL.fromStrict $
                 "Click this link to permanently delete your account: "
                 <> confirmUrl
      htmlBody = TL.fromStrict $
                 "<p>Click this link to permanently delete your account: \
                 \<a href=\"" <> confirmUrl <> "\">"
                 <> confirmUrl <> "</a></p>"

      from = Mail.Address (Just "anorby") (emailFrom config)
      to = Mail.Address Nothing toEmail

  if emailSend config
    then do
      mail <- Mail.simpleMail from to "" "" subject
        [(TL.toStrict textBody, "plain"), (TL.toStrict htmlBody, "html")]
      SMTP.sendMailWithLogin
        (emailHost config)
        (emailUser config)
        (emailPass config)
        mail
    else do
      putStrLn $
        "delete confirm url for "
        ++ T.unpack toEmail
        ++ ": "
        ++ T.unpack confirmUrl

-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  config <- getConfig
  pool <- case environment config of
    Production -> do
      initPool (dbPath config)
    TestWithAnswers -> do
      Monad.when (newDb config) $ do
        putStrLn "Creating new test database with answers..."
        conn <- initDB (dbPath config) True
        mockBaseAorbAnswers conn (userCount config)
        SQL.close conn
      initPool (dbPath config)
    TestWithoutAnswers -> do
      Monad.when (newDb config) $ do
        putStrLn "Creating new test database without answers..."
        conn <- initDB (dbPath config) True
        mockBase conn (userCount config)
        SQL.close conn
      initPool (dbPath config)
  maybePort <- Env.lookupEnv "PORT"
  let autoPort = 5001
      port = maybe autoPort read maybePort
  putStrLn $ "Server starting on port " ++ show (port :: Int)
  putStrLn $ "  Environment: " ++ show (environment config)
  putStrLn $ "  Database: " ++ dbPath config
  Warp.run port $ monolith pool
