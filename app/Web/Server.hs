{-# LANGUAGE OverloadedStrings #-}

module Web.Server where

import qualified Control.Monad as Monad
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.List as List
import qualified Data.Maybe as Maybe
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
import qualified Network.Wai.Middleware.Gzip as Gzip
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

-- | Server Runners
--
runServer :: IO ()
runServer = do
  config <- getConfig
  state <- initAppState config
  port <- getServerPort

  let settings = Warp.setServerName "anorby"
                  $ Warp.setPort port
                  $ Warp.setTimeout 60
                  $ Warp.setGracefulCloseTimeout1 1000
                  $ Warp.setGracefulCloseTimeout2 5000
                  $ Warp.setFdCacheDuration 600
                  $ Warp.setFileInfoCacheDuration 600
                  $ Warp.setSlowlorisSize 8192
                  $ Warp.setMaxTotalHeaderLength (1024*50)
                  $ Warp.setOnException (\_ e ->
                      Monad.when (Warp.defaultShouldDisplayException e) $
                        putStrLn $ "Error: " ++ show e)
                  $ Warp.setGracefulShutdownTimeout (Just 30)
                  $ Warp.defaultSettings

  putStrLn $ "Server starting on port " ++ show port
  putStrLn $ "  Environment: " ++ show (environment config)
  putStrLn $ "  Database: " ++ dbPath config
  Warp.runSettings settings $ monolith state

initAppState :: Config -> IO AppState
initAppState config = do
  pool <- initDatabasePool config
  rootCache <- initCache (60 * 60)
  statsCache <- initCache (5 * 60)
  htmlCache <- initCache (2 * 60)
  queryCache <- initCache 30
  matchState <- initMatchState
  return AppState
    { appPool = pool
    , appRootCache = rootCache
    , appStatsCache = statsCache
    , appHtmlCache = htmlCache
    , appQueryCache = queryCache
    , appMatchState = matchState
    }

getServerPort :: IO Int
getServerPort = maybe 5001 read <$> Env.lookupEnv "PORT"

-- | Database Runners

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

-- | Routing

pathMatches :: BS.ByteString -> BS.ByteString -> Bool
pathMatches pattern path =
  let patternParts = BS.split '/' pattern
      pathParts = BS.split '/' path
  in length patternParts == length pathParts &&
     and (zipWith matchPart patternParts pathParts)
  where
    matchPart p1 p2
      | BS.isPrefixOf "t-:" p1 = True
      | BS.elem ':' p1 = True
      | otherwise = p1 == p2

extractParam :: BS.ByteString -> BS.ByteString -> BS.ByteString -> Maybe BS.ByteString
extractParam pattern path paramName =
  let patternParts = BS.split '/' pattern
      pathParts = BS.split '/' path
      findParam p1 p2
        | BS.isPrefixOf "t-:" p1 = Just (BS.drop 2 p2)
        | BS.pack (':' : BS.unpack paramName) == p1 = Just p2
        | otherwise = Nothing
  in Maybe.listToMaybe $ Maybe.catMaybes $ zipWith findParam patternParts pathParts

data Route = Route
  { routeMethod :: BS.ByteString
  , routePath :: BS.ByteString
  , routeHandler :: RouteHandler
  }

routes :: [Route]
routes =
  -- Public routes
  [ Route "GET" "/styles/output.css" $ PublicHandler $ \_ _ _->
      serveStaticFile "/styles/output.css"
  , Route "GET" "/" $ PublicHandler rootTemplateRoute
  , Route "GET" "/share/:uuid" $ PublicHandler $ \_ conn req ->
      case extractParam "/share/:uuid" (Wai.rawPathInfo req) "uuid" of
        Just uuid -> sharedProfileTemplateRoute conn (T.pack $ BS.unpack uuid) req
        Nothing -> return notFoundResponse

  -- Auth routes
  , Route "GET" "/login" $ AuthHandler loginGetRoute
  , Route "POST" "/login" $ AuthHandler loginPostRoute
  , Route "GET" "/register" $ AuthHandler registerGetRoute
  , Route "POST" "/register" $ AuthHandler registerPostRoute
  , Route "GET" "/auth/:hash" $ AuthHandler $ \conn req ->
      case extractParam "/auth/:hash" (Wai.rawPathInfo req) "hash" of
        Just hash -> authHashRoute conn (T.pack $ BS.unpack hash) req
        Nothing -> return notFoundResponse

  -- Admin routes (protected)
  , Route "GET" "/admin" $ ProtectedHandler $ \_ _ conn uid req ->
      adminTemplateRoute conn uid req
  , Route "POST" "/admin/aorb/add" $ ProtectedHandler $ \config _ conn uid req ->
      handleAddAorb config conn uid req
  , Route "GET" "/admin/aorb/:id/edit" $ ProtectedHandler $ \config _ conn uid req ->
      case extractParam "/admin/aorb/:id/edit" (Wai.rawPathInfo req) "id" of
        Just idBS -> case reads (BS.unpack idBS) of
          [(id', "")] -> handleEditAorbForm config conn uid id' req
          _ -> return notFoundResponse
        Nothing -> return notFoundResponse
  , Route "POST" "/admin/aorb/:id/edit" $ ProtectedHandler $ \config _ conn uid req ->
      case extractParam "/admin/aorb/:id/edit" (Wai.rawPathInfo req) "id" of
        Just idBS -> case reads (BS.unpack idBS) of
          [(id', "")] -> handleEditAorb config conn uid id' req
          _ -> return notFoundResponse
        Nothing -> return notFoundResponse
  , Route "POST" "/admin/aorb/:id/delete" $ ProtectedHandler $ \config _ conn uid req ->
      case extractParam "/admin/aorb/:id/delete" (Wai.rawPathInfo req) "id" of
        Just idBS -> case reads (BS.unpack idBS) of
          [(id', "")] -> handleDeleteAorb config conn uid id' req
          _ -> return notFoundResponse
        Nothing -> return notFoundResponse

  -- User routes (protected)
  , Route "GET" "/whoami" $ ProtectedHandler $ \config _ conn uid req ->
      profileTemplateRoute config conn uid req
  , Route "GET" "/account" $ ProtectedHandler $ \_ _ conn uid req ->
      accountTemplateRoute conn uid req

  -- Answer routes (protected)
  , Route "GET" "/ans" $ ProtectedHandler $ \_ _ conn uid req ->
      ansTemplateRoute conn uid req
  , Route "GET" "/ans/:id" $ ProtectedHandler $ \_ _ conn uid req ->
        case extractParam "/ans/:id" (Wai.rawPathInfo req) "id" of
          Just idBS -> case reads (BS.unpack idBS) of
            [(id', "")] -> existingAnswerTemplateRoute conn uid id' req
            _ -> return notFoundResponse
          Nothing -> return notFoundResponse
  , Route "POST" "/ans/submit" $ ProtectedHandler $ \_ state conn uid req ->
      handleAnswerSubmission state conn uid req
  , Route "POST" "/ans/edit" $ ProtectedHandler $ \_ state conn uid req ->
      handleAnswerEdit state conn uid req
  , Route "POST" "/aorb/favorite/:id" $ ProtectedHandler $ \_ _ conn uid req ->
      case extractParam "/aorb/favorite/:id" (Wai.rawPathInfo req) "id" of
        Just idBS -> case reads (BS.unpack idBS) of
          [(id', "")] -> setFavoriteAorbRoute conn uid id' req
          _ -> return notFoundResponse
        Nothing -> return notFoundResponse

  -- Match routes (protected)
  , Route "GET" "/match" $ ProtectedHandler $ \config state conn uid req ->
      matchTemplateRoute config state conn uid req
  , Route "GET" "/match/type" $ ProtectedHandler $ \config _ conn uid req ->
      matchTypeTemplateRoute config conn uid req
  , Route "POST" "/match/type" $ ProtectedHandler $ \config _ conn uid req ->
      matchTypeUpdateRoute config conn uid req
  , Route "GET" "/match/found" $ ProtectedHandler $ \config _ conn uid req ->
      matchFoundTemplateRoute config conn uid req
  , Route "GET" "/match/found/t-:days" $ ProtectedHandler $ \config _ conn uid req ->
      case extractParam "/match/found/t-:days" (Wai.rawPathInfo req) "days" of
        Just daysBS -> case reads (BS.unpack daysBS) of
          [(days, "")] -> matchProfileTemplateRoute config conn uid days req
          _ -> return notFoundResponse
        Nothing -> return notFoundResponse
  , Route "POST" "/match/found/t-:days/message" $ ProtectedHandler $ \config _ conn uid req ->
      case extractParam "/match/found/t-:days/message" (Wai.rawPathInfo req) "days" of
        Just daysBS -> case reads (BS.unpack daysBS) of
          [(days, "")] -> postMessageRoute config conn uid days req
          _ -> return notFoundResponse
        Nothing -> return notFoundResponse

  -- Account management routes (protected)
  , Route "GET" "/logout" $ ProtectedHandler $ \_ _ conn uid req ->
      logoutGetRoute conn uid req
  , Route "GET" "/logout/confirm" $ ProtectedHandler $ \_ _ conn uid req ->
      logoutConfirmRoute conn uid req
  , Route "POST" "/logout/confirm" $ ProtectedHandler $ \_ _ conn uid req ->
      logoutConfirmPostRoute conn uid req
  , Route "GET" "/delete" $ ProtectedHandler $ \_ _ conn uid req ->
      deleteGetRoute conn uid req
  , Route "GET" "/delete/confirm" $ ProtectedHandler $ \_ _ conn uid req ->
      deleteConfirmRoute conn uid req
  , Route "POST" "/delete/confirm" $ ProtectedHandler $ \_ _ conn uid req ->
      deleteConfirmPostRoute conn uid req
  ]

-- | Server

monolith :: AppState -> Wai.Application
monolith state = Gzip.gzip Gzip.defaultGzipSettings $ Mid.logStdout $ application TIO.putStrLn state

data RouteHandler
  = PublicHandler (AppState -> SQL.Connection -> Wai.Request -> IO Wai.Response)
  | ProtectedHandler (Config -> AppState -> SQL.Connection -> UserID -> Wai.Request -> IO Wai.Response)
  | AuthHandler (SQL.Connection -> Wai.Request -> IO Wai.Response)

application :: Logger -> AppState -> Wai.Application
application _ state request respond = do
  config <- getConfig
  checkAndTriggerMatching state config

  let method = Wai.requestMethod request
      path = Wai.rawPathInfo request

      matchingRoute =
        List.find
          (\r -> routeMethod r == method && pathMatches (routePath r) path)
          routes

  case matchingRoute of
    Nothing -> respond notFoundResponse
    Just route -> case routeHandler route of
      PublicHandler h ->
        Pool.withResource (appPool state) (\conn -> h state conn request) >>= respond

      AuthHandler h ->
        Pool.withResource (appPool state) (\conn -> h conn request) >>= respond

      ProtectedHandler h ->
        Pool.withResource (appPool state) (\conn -> do
          maybeUser <- getAuthenticatedUser conn request
          case maybeUser of
            Nothing -> return redirectToLogin
            Just user -> do
              updateLastAccessed conn request
              h config state conn (userId user) request
        ) >>= respond

data AnswerSubmission = AnswerSubmission
  { submissionAorbId :: AorbID
  , submissionChoice :: Word.Word8
  , submissionToken :: T.Text
  }

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

routeAnswerSubmit :: AnswerSubmission -> SQL.Connection -> UserID -> Wai.Request -> IO Wai.Response
routeAnswerSubmit submission conn uid req =
  submitAnswerRoute conn uid
    (submissionAorbId submission)
    (AorbAnswer $ submissionChoice submission)
    (submissionToken submission)
    req

routeAnswerEdit :: AnswerSubmission -> SQL.Connection -> UserID -> Wai.Request -> IO Wai.Response
routeAnswerEdit submission conn uid req =
  editAnswerRoute conn uid
    (submissionAorbId submission)
    (AorbAnswer $ submissionChoice submission)
    (submissionToken submission)
    req

handleAnswerSubmission :: AppState -> SQL.Connection -> UserID -> Wai.Request -> IO Wai.Response
handleAnswerSubmission _ conn uid req = do
  body <- Wai.strictRequestBody req
  case parseAnswerBody body of
    Just submission -> routeAnswerSubmit submission conn uid req
    Nothing -> return invalidSubmissionResponse

handleAnswerEdit :: AppState -> SQL.Connection -> UserID -> Wai.Request -> IO Wai.Response
handleAnswerEdit _ conn uid req = do
  body <- Wai.strictRequestBody req
  case parseAnswerBody body of
    Just submission -> routeAnswerEdit submission conn uid req
    Nothing -> return invalidSubmissionResponse

updateLastAccessed :: SQL.Connection -> Wai.Request -> IO ()
updateLastAccessed conn req = do
  now <- POSIXTime.getPOSIXTime
  case getCookie req of
    Just cookieBS -> do
      let hash = TE.decodeUtf8 cookieBS
      SQL.execute conn
        "UPDATE auth SET last_accessed = ? WHERE hash = ?"
        (floor now :: Integer, hash)
    Nothing -> return ()
