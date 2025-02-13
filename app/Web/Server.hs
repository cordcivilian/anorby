{-# LANGUAGE OverloadedStrings #-}

module Web.Server where

import qualified Control.Monad as Monad
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Pool as Pool
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import qualified Data.Text.IO as TIO
import qualified Data.Time.Clock.POSIX as POSIXTime
import qualified Data.Word as Word
import qualified Database.SQLite.Simple as SQL
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Middleware.RequestLogger as Mid
import qualified System.Directory as Dir
import qualified System.Environment as Env
import qualified Text.Read as Read

import Auth
import Database
import Utils.Cache
import Utils.Config
import Utils.Simulate
import Utils.MatchState
import Utils.MatchTrigger
import Web.Types
import Web.Handlers
import Core.RollingShadow
import Types

type Route = (BS.ByteString, BS.ByteString)

data AnswerSubmission = AnswerSubmission
  { submissionAorbId :: AorbID
  , submissionChoice :: Word.Word8
  , submissionToken :: T.Text
  }

type PublicRouteHandler =
  Route -> SQL.Connection -> Wai.Request -> IO Wai.Response

type ProtectedRouteHandler =
  Route -> SQL.Connection -> UserID -> Wai.Request -> IO Wai.Response

type AuthRouteHandler =
  Route -> SQL.Connection -> Wai.Request -> IO Wai.Response

runServer :: IO ()
runServer = do
  config <- getConfig
  state <- initAppState config
  port <- getServerPort
  putStrLn $ "Server starting on port " ++ show port
  putStrLn $ "  Environment: " ++ show (environment config)
  putStrLn $ "  Database: " ++ dbPath config
  Warp.run port $ monolith state

initAppState :: Config -> IO AppState
initAppState config = do
  pool <- initDatabasePool config
  rootCache <- initCache (60)
  matchState <- initMatchState
  return AppState
    { appPool = pool
    , appRootCache = rootCache
    , appMatchState = matchState
    }

getServerPort :: IO Int
getServerPort = maybe 5001 read <$> Env.lookupEnv "PORT"

initDatabasePool :: Config -> IO (Pool.Pool SQL.Connection)
initDatabasePool config = do
  dbExists <- Dir.doesFileExist (dbPath config)
  case environment config of
    Production -> initProductionDb config dbExists
    TestWithAnswers -> initTestWithAnswersDb config dbExists
    TestWithoutAnswers -> initTestWithoutAnswersDb config dbExists
    TestWithCustomData -> initPool (dbPath config)
    TestWithMatches 1 -> initGaleShapleyTestDb config dbExists
    TestWithMatches _ -> initLocalSearchTestDb config dbExists

initProductionDb :: Config -> Bool -> IO (Pool.Pool SQL.Connection)
initProductionDb config dbExists = do
  Monad.when (not dbExists) $ do
    putStrLn "Creating new production database..."
    conn <- initDB (dbPath config) True
    ensureShadowUser conn
    SQL.close conn
  initPool (dbPath config)

initTestWithAnswersDb :: Config -> Bool -> IO (Pool.Pool SQL.Connection)
initTestWithAnswersDb config dbExists = do
  Monad.when (newDb config || not dbExists) $ do
    putStrLn "Creating new test database with answers..."
    conn <- initDB (dbPath config) True
    mockBaseAorbAnswers conn (userCount config)
    SQL.close conn
  initPool (dbPath config)

initTestWithoutAnswersDb :: Config -> Bool -> IO (Pool.Pool SQL.Connection)
initTestWithoutAnswersDb config dbExists = do
  Monad.when (newDb config || not dbExists) $ do
    putStrLn "Creating new test database without answers..."
    conn <- initDB (dbPath config) True
    mockBase conn (userCount config)
    SQL.close conn
  initPool (dbPath config)

initGaleShapleyTestDb :: Config -> Bool -> IO (Pool.Pool SQL.Connection)
initGaleShapleyTestDb config dbExists = do
  Monad.when (newDb config || not dbExists) $ do
    putStrLn "Creating new test database with Gale-Shapley matching..."
    conn <- initDB (dbPath config) True
    mockBaseAorbAnswersWithGaleShapley conn (userCount config)
    SQL.close conn
  initPool (dbPath config)

initLocalSearchTestDb :: Config -> Bool -> IO (Pool.Pool SQL.Connection)
initLocalSearchTestDb config dbExists = do
  Monad.when (newDb config || not dbExists) $ do
    putStrLn "Creating new test database with Local Search matching..."
    conn <- initDB (dbPath config) True
    mockBaseAorbAnswersWithLocalSearch conn (userCount config)
    SQL.close conn
  initPool (dbPath config)

routePublic
  :: AppState -> Route -> SQL.Connection -> Wai.Request
  -> IO Wai.Response
routePublic state (method, path) conn req =
  case (method, path) of
    ("GET", p) | isStylesPath p -> do
      serveStaticFile p
    ("GET", "/") -> rootTemplateRoute state conn req
    ("GET", p) | isSharePath p ->
      sharedProfileTemplateRoute conn (extractUuid p) req
    _ -> return notFoundResponse
  where
    isSharePath = BS.isPrefixOf "/share/"
    isStylesPath = BS.isPrefixOf "/styles/"
    extractUuid = T.pack . BS.unpack . BS.drop 7

routeAuth :: Route -> SQL.Connection -> Wai.Request -> IO Wai.Response
routeAuth (method, path) conn req = case (method, path) of
  ("GET", "/login") -> loginGetRoute conn req
  ("POST", "/login") -> loginPostRoute conn req
  ("GET", "/register") -> registerGetRoute conn req
  ("POST", "/register") -> registerPostRoute conn req
  ("GET", p) | isAuthPath p -> authHashRoute conn (extractHash p) req
  _ -> return notFoundResponse
  where
    isAuthPath = BS.isPrefixOf "/auth/"
    extractHash = T.pack . BS.unpack . BS.drop 6

routeProtected
  :: Config -> AppState -> Route -> SQL.Connection -> UserID -> Wai.Request
  -> IO Wai.Response
routeProtected config state (method, path) conn uid req =
  case (method, path) of

  ("GET", "/admin") | uid == shadowUserId -> adminTemplateRoute conn uid req
  ("POST", "/admin/aorb/add") -> handleAddAorb config conn uid req
  ("GET", p) | isEditAorbPath p -> handleEditAorbForm config conn uid (extractAorbId p) req
  ("POST", p) | isEditAorbPath p -> handleEditAorb config conn uid (extractAorbId p) req
  ("POST", p) | isDeleteAorbPath p -> handleDeleteAorb config conn uid (extractAorbId p) req

  ("GET", "/whoami") -> profileTemplateRoute config conn uid req
  ("GET", "/account") -> accountTemplateRoute conn uid req

  ("GET", "/ans") -> ansTemplateRoute conn uid req
  ("GET", p) | isAorbPath p ->
    existingAnswerTemplateRoute conn uid (extractAorbId p) req
  ("POST", p) | isFavoritePath p ->
    setFavoriteAorbRoute conn uid (extractFavoriteAorbId p) req

  ("GET", "/match") -> matchTemplateRoute config state conn uid req
  ("GET", "/match/type") -> matchTypeTemplateRoute config conn uid req
  ("POST", "/match/type") -> matchTypeUpdateRoute config conn uid req
  ("GET", "/match/found") -> matchFoundTemplateRoute config conn uid req
  ("GET", p) | isMatchDaysPath p ->
    matchProfileTemplateRoute config conn uid (extractDays p) req
  ("POST", p) | isMatchMessagePath p ->
    postMessageRoute config conn uid (extractDays p) req

  ("GET", "/logout") -> logoutGetRoute conn uid req
  ("GET", "/logout/confirm") -> logoutConfirmRoute conn uid req
  ("POST", "/logout/confirm") -> logoutConfirmPostRoute conn uid req
  ("GET", "/delete") -> deleteGetRoute conn uid req
  ("GET", "/delete/confirm") -> deleteConfirmRoute conn uid req
  ("POST", "/delete/confirm") -> deleteConfirmPostRoute conn uid req

  _ -> return notFoundResponse
  where
    isAorbPath = BS.isPrefixOf "/ans/"
    isFavoritePath = BS.isPrefixOf "/aorb/favorite/"
    isMatchDaysPath = BS.isPrefixOf "/match/found/t-"
    isMatchMessagePath p = BS.isPrefixOf "/match/found/t-" p && BS.isSuffixOf "/message" p
    isEditAorbPath p = BS.isPrefixOf "/admin/aorb/" p && BS.isSuffixOf "/edit" p
    isDeleteAorbPath p = BS.isPrefixOf "/admin/aorb/" p && BS.isSuffixOf "/delete" p

    extractAorbId :: BS.ByteString -> AorbID
    extractAorbId p =
      let parts = BS.split '/' p
          idPart = if length parts >= 4
                   then parts !! 3
                   else "0"
      in case reads (BS.unpack idPart) of
           [(n, "")] -> n
           _ -> 0
    extractFavoriteAorbId p = read . BS.unpack . BS.drop 15 $ p

    extractDays p =
      let base = BS.drop 15 p
          days = if BS.isSuffixOf "/message" base
                    then BS.take (BS.length base - 8) base
                    else base
      in read . BS.unpack $ days

monolith :: AppState -> Wai.Application
monolith state = Mid.logStdout $ application TIO.putStrLn state

application :: Logger -> AppState -> Wai.Application
application _ state request respond = do
  config <- getConfig
  checkAndTriggerMatching state config

  _ <- Wai.lazyRequestBody request

  let method = Wai.requestMethod request
      path = Wai.rawPathInfo request
      route = (method, path)
      handleAuthRoute r = runHandlerWithConn $ routeAuth r
      handlePublicRoute r = runHandlerWithConn $ routePublic state r
      handleProtectedRoute r =
        runProtectedHandlerWithConn $ routeProtected config state r

  case route of

    ("GET", "/styles/output.css") -> handlePublicRoute route
    ("GET", "/") -> handlePublicRoute route

    ("GET", "/login") -> handleAuthRoute route
    ("POST", "/login") -> handleAuthRoute route
    ("GET", "/register") -> handleAuthRoute route
    ("POST", "/register") -> handleAuthRoute route
    ("GET", p) | isAuthHashPath p -> handleAuthRoute route

    ("GET", "/admin") -> handleProtectedRoute route
    ("POST", "/admin/aorb/add") -> handleProtectedRoute route
    ("GET", p) | isEditAorbPath p -> handleProtectedRoute route
    ("POST", p) | isEditAorbPath p -> handleProtectedRoute route
    ("POST", p) | isDeleteAorbPath p -> handleProtectedRoute route

    ("GET", p) | isSharePath p -> handlePublicRoute route
    ("GET", "/whoami") -> handleProtectedRoute route
    ("GET", "/account") -> handleProtectedRoute route

    ("GET", "/ans") -> handleProtectedRoute route
    ("GET", p) | isAorbPath p -> handleProtectedRoute route
    ("POST", "/ans/submit") -> handleAnswerSubmission
    ("POST", "/ans/edit") -> handleAnswerEdit
    ("POST", p) | isFavoriteAorbPath p -> handleProtectedRoute route

    ("GET", "/match") -> handleProtectedRoute route
    ("GET", "/match/type") -> handleProtectedRoute route
    ("POST", "/match/type") -> handleProtectedRoute route
    ("GET", "/match/found") -> handleProtectedRoute route
    ("GET", p) | isMatchDaysPath p -> handleProtectedRoute route
    ("POST", p) | isMatchMessagePath p -> handleProtectedRoute route

    ("GET", "/logout") -> handleProtectedRoute route
    ("GET", "/logout/confirm") -> handleProtectedRoute route
    ("POST", "/logout/confirm") -> handleProtectedRoute route
    ("GET", "/delete") -> handleProtectedRoute route
    ("GET", "/delete/confirm") -> handleProtectedRoute route
    ("POST", "/delete/confirm") -> handleProtectedRoute route

    _ -> respond notFoundResponse

  where
    isAuthHashPath = BS.isPrefixOf "/auth/"
    isSharePath = BS.isPrefixOf "/share/"
    isAorbPath = BS.isPrefixOf "/ans/"
    isFavoriteAorbPath = BS.isPrefixOf "/aorb/favorite/"
    isMatchDaysPath = BS.isPrefixOf "/match/found/t-"
    isMatchMessagePath p = BS.isPrefixOf "/match/found/t-" p && BS.isSuffixOf "/message" p
    isEditAorbPath p = BS.isPrefixOf "/admin/aorb/" p && BS.isSuffixOf "/edit" p
    isDeleteAorbPath p = BS.isPrefixOf "/admin/aorb/" p && BS.isSuffixOf "/delete" p


    runHandlerWithConn handler =
      Pool.withResource (appPool state) (\conn ->
        handler conn request) >>= respond

    runProtectedHandlerWithConn handler =
      Pool.withResource (appPool state) (\conn -> do
        maybeUser <- getAuthenticatedUser conn request
        case maybeUser of
          Nothing -> return redirectToLogin
          Just user -> do
            updateLastAccessed conn user
            handler conn (userId user) request
      ) >>= respond

    parseAnswerBody :: BSL.ByteString -> Maybe AnswerSubmission
    parseAnswerBody body = do
      let params = HTTP.parseQueryText $ BSL.toStrict $
            BSL.fromStrict $ TE.encodeUtf8 $
              TE.decodeUtf8With TEE.lenientDecode $ BSL.toStrict body
      aorb <- Monad.join (lookup "aorb_id" params) >>=
        Read.readMaybe . T.unpack
      choice <- Monad.join (lookup "choice" params) >>=
        Read.readMaybe . T.unpack
      token <- Monad.join (lookup "token" params)
      return AnswerSubmission
        { submissionAorbId = aorb
        , submissionChoice = choice
        , submissionToken = token
        }
    routeAnswerSubmit
      :: AnswerSubmission -> SQL.Connection -> UserID -> Wai.Request
      -> IO Wai.Response
    routeAnswerSubmit submission conn uid req =
      submitAnswerRoute conn uid
        (submissionAorbId submission)
        (AorbAnswer $ submissionChoice submission)
        (submissionToken submission)
        req
    routeAnswerEdit
      :: AnswerSubmission -> SQL.Connection -> UserID -> Wai.Request
      -> IO Wai.Response
    routeAnswerEdit submission conn uid req =
      editAnswerRoute conn uid
        (submissionAorbId submission)
        (AorbAnswer $ submissionChoice submission)
        (submissionToken submission)
        req
    handleAnswerSubmission = do
      body <- Wai.strictRequestBody request
      case parseAnswerBody body of
        Just submission -> runProtectedHandlerWithConn $
          routeAnswerSubmit submission
        Nothing -> respond invalidSubmissionResponse

    handleAnswerEdit = do
      body <- Wai.strictRequestBody request
      case parseAnswerBody body of
        Just submission -> runProtectedHandlerWithConn $
          routeAnswerEdit submission
        Nothing -> respond invalidSubmissionResponse

    updateLastAccessed conn _ = do
      now <- POSIXTime.getPOSIXTime
      case getCookie request of
        Just cookieBS -> do
          let hash = TE.decodeUtf8 cookieBS
          SQL.execute conn
            "UPDATE auth SET last_accessed = ? WHERE hash = ?"
            (floor now :: Integer, hash)
        Nothing -> return ()
