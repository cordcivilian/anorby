{-# LANGUAGE OverloadedStrings #-}

module Web.Server where

import qualified Network.HTTP.Types as HTTP
import qualified Data.ByteString.Char8 as BS
import qualified Data.Time.Clock as Clock
import qualified Data.List as List
import qualified Control.Monad as Monad
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Pool as Pool
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import qualified Text.Read as Read
import qualified Data.Word as Word
import qualified Data.Time.Clock.POSIX as POSIXTime
import qualified Database.SQLite.Simple as SQL
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Middleware.RequestLogger as Mid
import qualified System.Directory as Dir
import qualified System.Environment as Env
import qualified System.IO.Unsafe as Unsafe

import Auth
import Database
import Utils.Cache
import Utils.Config
import Utils.Simulate
import Web.Types
import Web.Handlers
import Types

-- | Server setup and initialization

runServer :: IO ()
runServer = do
  config <- getConfig
  pool <- initDatabasePool config
  maybePort <- Env.lookupEnv "PORT"
  let autoPort = 5001
      port = maybe autoPort read maybePort
  putStrLn $ "Server starting on port " ++ show (port :: Int)
  putStrLn $ "  Environment: " ++ show (environment config)
  putStrLn $ "  Database: " ++ dbPath config
  Warp.run port $ monolith pool

initDatabasePool :: Config -> IO (Pool.Pool SQL.Connection)
initDatabasePool config =
  Dir.doesFileExist (dbPath config) >>= \dbExists ->
    case environment config of
      Production -> do
        Monad.when (not dbExists) $ do
          putStrLn "Creating new production database..."
          conn <- initDB (dbPath config) True
          SQL.close conn
        initPool (dbPath config)
      TestWithAnswers -> do
        Monad.when (newDb config || not dbExists) $ do
          putStrLn "Creating new test database with answers..."
          conn <- initDB (dbPath config) True
          mockBaseAorbAnswers conn (userCount config)
          SQL.close conn
        initPool (dbPath config)
      TestWithoutAnswers -> do
        Monad.when (newDb config || not dbExists) $ do
          putStrLn "Creating new test database without answers..."
          conn <- initDB (dbPath config) True
          mockBase conn (userCount config)
          SQL.close conn
        initPool (dbPath config)
      TestWithCustomData -> do
        initPool (dbPath config)

-- | Application setup

monolith :: Pool.Pool SQL.Connection -> Wai.Application
monolith pool =
  let rootCache = Unsafe.unsafePerformIO $
        initCache (60 * Clock.secondsToNominalDiffTime 1)
      state = AppState
        { appPool = pool
        , appRootCache = rootCache
        }
  in Mid.logStdout $ application TIO.putStrLn state

application :: Logger -> AppState -> Wai.Application
application _ state request respond = do

  _ <- Wai.lazyRequestBody request

  config <- getConfig

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
            Just user -> do
              now <- POSIXTime.getPOSIXTime
              let timestamp = floor now :: Integer
              case getCookie request of
                Just cookieBS -> do
                  let hash = TE.decodeUtf8 cookieBS
                  SQL.execute conn
                    "UPDATE auth SET last_accessed = ? WHERE hash = ?"
                    (timestamp, hash)
                Nothing -> return ()
              handler conn (userId user) request) >>= respond

      method = BS.unpack $ Wai.requestMethod request
      path = BS.unpack $ Wai.rawPathInfo request
      pool = appPool state

  case (method, path) of
    -- Public routes
    ("GET", "/") ->
      runHandlerWithConn (rootTemplateRoute state)

    ("GET", "/roadmap") ->
      runHandlerWithConn roadmapTemplateRoute

    ("GET", "/login") ->
      runHandlerWithConn loginGetRoute

    ("POST", "/login") ->
      runHandlerWithConn loginPostRoute

    ("GET", "/register") ->
      runHandlerWithConn registerGetRoute

    ("POST", "/register") ->
      runHandlerWithConn registerPostRoute

    ("GET", p) | Just h <- List.stripPrefix "/auth/" p ->
      runHandlerWithConn (\conn -> authHashRoute conn (T.pack h))

    ("GET", p) | Just uuid <- List.stripPrefix "/share/" p ->
      runHandlerWithConn (\conn ->
        sharedProfileTemplateRoute conn (T.pack uuid))

    -- Protected routes
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

    ("GET", "/whoami") ->
      runProtectedHandlerWithConn (\conn uid ->
        profileTemplateRoute config conn uid)

    ("GET", "/ans") ->
      runProtectedHandlerWithConn ansTemplateRoute

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

    ("GET", "/match") ->
      runProtectedHandlerWithConn (\conn uid ->
        matchTemplateRoute config conn uid)

    ("GET", "/match/type") ->
      runProtectedHandlerWithConn (\conn uid ->
        matchTypeTemplateRoute config conn uid)

    ("POST", "/match/type") ->
      runProtectedHandlerWithConn (\conn uid ->
        matchTypeUpdateRoute config conn uid)

    _ -> respond notFoundResponse

  where
    readAorbId :: String -> Maybe AorbID
    readAorbId path = Read.readMaybe =<< List.stripPrefix "/ans/" path

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
