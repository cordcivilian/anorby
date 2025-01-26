{-# LANGUAGE OverloadedStrings #-}

module Web.Handlers where

import qualified Control.Monad as Monad
import qualified Data.ByteString.Char8 as BS
import qualified Data.Maybe as Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Time.Clock.POSIX as POSIXTime
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import qualified Database.SQLite.Simple as SQL
import qualified Network.HTTP.Types as HTTP
import qualified Network.HTTP.Types.Header as Headers
import qualified Network.Wai as Wai
import qualified Network.Wai.Parse as Wai
import qualified System.Random as Random
import qualified Text.Blaze.Html.Renderer.Utf8 as R

import Auth
import Core.Matching
import Core.Similarity
import Database
import Types
import Utils.Config
import Utils.Cache
import Utils.Email
import Utils.Time
import Web.Templates
import Web.Types

-- | Public Route Handlers

rootTemplateRoute :: AppState -> SQL.Connection -> Wai.Request
                  -> IO Wai.Response
rootTemplateRoute state conn _ = do
  cachedAorbs <- getFromCache "root_aorbs" (appRootCache state)
  aorbs <- case cachedAorbs of
    Just as -> return as
    Nothing -> do
      as <- SQL.query_ conn "SELECT * FROM aorb" :: IO [Aorb]
      gen <- Random.getStdGen
      let (shuffledAorbs, _) = fisherYatesShuffle gen as
      putInCache "root_aorbs" shuffledAorbs (appRootCache state)
      return shuffledAorbs
  activeUsers <- SQL.query_ conn
    "SELECT COUNT(DISTINCT user_id) FROM aorb_answers" :: IO [SQL.Only Int]
  let totalActiveUsers = maybe 0 SQL.fromOnly (Maybe.listToMaybe activeUsers)
  return $ Wai.responseLBS
    HTTP.status200
    [(Headers.hContentType, BS.pack "text/html")]
    (R.renderHtml $ rootTemplate totalActiveUsers aorbs)

roadmapTemplateRoute :: SQL.Connection -> Wai.Request -> IO Wai.Response
roadmapTemplateRoute conn _ = do
  activeUsers <- SQL.query_ conn
    "SELECT COUNT(DISTINCT user_id) FROM aorb_answers" :: IO [SQL.Only Int]
  let totalActiveUsers = maybe 0 SQL.fromOnly (Maybe.listToMaybe activeUsers)
  return $ Wai.responseLBS
    HTTP.status200
    [(Headers.hContentType, BS.pack "text/html")]
    (R.renderHtml $ roadmapTemplate totalActiveUsers)

profileTemplateRoute :: Config -> SQL.Connection -> UserID -> Wai.Request
                     -> IO Wai.Response
profileTemplateRoute config conn uid _ = do
  hasAccess <- hasThresholdAccess conn uid (profileThreshold config)
  userQ <- SQL.query conn "SELECT * FROM users WHERE id = ?" (SQL.Only uid)
  case userQ of
    (user:_) -> do
      if not hasAccess
        then return $ Wai.responseLBS
          HTTP.status200
          [(Headers.hContentType, BS.pack "text/html")]
          (R.renderHtml $ profileNotYetActive (profileThreshold config))
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
              profileTemplate myAorbs (userAorbId user) Nothing shareUrl)
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
          profileTemplate myAorbs (userAorbId user) (Just uuid) Nothing)
    Nothing -> return $ Wai.responseLBS
      HTTP.status404
      [(Headers.hContentType, BS.pack "text/html")]
      (R.renderHtml notFoundTemplate)

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

existingAnswerTemplateRoute :: SQL.Connection -> UserID -> AorbID
                           -> Wai.Request -> IO Wai.Response
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
          (R.renderHtml invalidTokenTemplate)
        else do
          existing <- SQL.query conn
            "SELECT 1 FROM aorb_answers WHERE user_id = ? AND aorb_id = ?"
            (uid, aid) :: IO [SQL.Only Int]
          case existing of
            (_:_) -> return $ Wai.responseLBS
              HTTP.status403
              [(Headers.hContentType, BS.pack "text/html")]
              (R.renderHtml alreadyAnsweredTemplate)
            [] -> do
              now <- POSIXTime.getPOSIXTime
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
      (R.renderHtml invalidTokenTemplate)
    else do
      now <- POSIXTime.getPOSIXTime
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
        ( aorbAnswer ans
        , show $ aorbAnsweredOn ans
        , aorbUserId ans
        , aorbAorbId ans
        )
      return $ Wai.responseLBS
        HTTP.status303
        [ (Headers.hLocation, BS.pack $ "/ans/" ++ show aid)
        , (Headers.hContentType, BS.pack "text/html")
        ]
        ""

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

matchTemplateRoute :: Config -> SQL.Connection -> UserID -> Wai.Request
                   -> IO Wai.Response
matchTemplateRoute config conn uid _ = do
  hasAccess <- hasThresholdAccess conn uid (matchThreshold config)
  if not hasAccess
    then return $ Wai.responseLBS
      HTTP.status200
      [(Headers.hContentType, BS.pack "text/html")]
      (R.renderHtml $ matchNotYetActive (matchThreshold config))
    else do
      assocResult <- SQL.query conn
        "SELECT assoc FROM users WHERE id = ?"
        (SQL.Only uid) :: IO [SQL.Only (Maybe AssociationScheme)]
      case assocResult of
        [] -> return $ Wai.responseLBS
          HTTP.status404
          [(Headers.hContentType, BS.pack "text/html")]
          (R.renderHtml notFoundTemplate)
        [SQL.Only Nothing] -> return $ Wai.responseLBS
          HTTP.status303
          [ (Headers.hLocation, "/match/type")
          , (Headers.hContentType, "text/html")
          ]
          ""
        [SQL.Only (Just _)] -> do
          answerCount <- getDailyAnswerCount conn uid
          [SQL.Only totalQuestions] <- SQL.query_ conn
            "SELECT COUNT(*) FROM aorb" :: IO [SQL.Only Int]
          enrolledCount <- getEnrolledCount conn
          Monad.when (answerCount >= 10 || answerCount >= totalQuestions) $ do
            insertOrUpdatePreMatch conn uid
          return $ Wai.responseLBS
            HTTP.status200
            [(Headers.hContentType, BS.pack "text/html")]
            (R.renderHtml $ matchTemplate answerCount totalQuestions enrolledCount "20:00")
        _ -> return $ Wai.responseLBS
          HTTP.status500
          [(Headers.hContentType, BS.pack "text/html")]
          (R.renderHtml internalErrorTemplate)

matchTypeTemplateRoute :: Config -> SQL.Connection -> UserID -> Wai.Request
                      -> IO Wai.Response
matchTypeTemplateRoute config conn uid _ = do
  hasAccess <- hasThresholdAccess conn uid (matchThreshold config)
  if not hasAccess
    then return $ Wai.responseLBS
      HTTP.status200
      [(Headers.hContentType, BS.pack "text/html")]
      (R.renderHtml $ matchNotYetActive (matchThreshold config))
    else do
      userQ <- SQL.query conn "SELECT * FROM users WHERE id = ?" (SQL.Only uid)
      case userQ of
        (user:_) -> return $ Wai.responseLBS
          HTTP.status200
          [(Headers.hContentType, BS.pack "text/html")]
          (R.renderHtml $ matchTypeTemplate user)
        [] -> return $ Wai.responseLBS
          HTTP.status404
          [(Headers.hContentType, BS.pack "text/html")]
          (R.renderHtml notFoundTemplate)

matchTypeUpdateRoute :: Config -> SQL.Connection -> UserID -> Wai.Request
                    -> IO Wai.Response
matchTypeUpdateRoute config conn uid req = do
  hasAccess <- hasThresholdAccess conn uid (matchThreshold config)
  if not hasAccess
    then return $ Wai.responseLBS
      HTTP.status200
      [(Headers.hContentType, BS.pack "text/html")]
      (R.renderHtml $ matchNotYetActive (matchThreshold config))
    else do
      (params, _) <- Wai.parseRequestBody Wai.lbsBackEnd req
      case lookup "assoc" params of
        Just assocBS -> do
          let assocText = TE.decodeUtf8 assocBS
          case parseAssociationScheme assocText of
            Just scheme -> do
              SQL.execute conn
                "UPDATE users SET assoc = ? WHERE id = ?"
                (scheme, uid)
              return $ Wai.responseLBS
                HTTP.status303
                [ (Headers.hLocation, "/match/type")
                , (Headers.hContentType, "text/html")
                ]
                ""
            Nothing -> return invalidSubmissionResponse
        Nothing -> return invalidSubmissionResponse
  where
    parseAssociationScheme :: T.Text -> Maybe AssociationScheme
    parseAssociationScheme "PPPod" = Just PPPod
    parseAssociationScheme "Fencer" = Just Fencer
    parseAssociationScheme "Bipolar" = Just Bipolar
    parseAssociationScheme _ = Nothing

matchFoundTemplateRoute :: SQL.Connection -> UserID -> Wai.Request
                        -> IO Wai.Response
matchFoundTemplateRoute conn uid _ = do
  matches <- getUserMatches conn uid
  matchScores <- mapM (calculateMatchScore conn uid) matches
  return $ Wai.responseLBS
    HTTP.status200
    [(Headers.hContentType, BS.pack "text/html")]
    (R.renderHtml $ matchFoundTemplate matchScores)
  where
    calculateMatchScore :: SQL.Connection -> UserID -> Match
                        -> IO (Match, Double)
    calculateMatchScore conn' uid' match' = do
      let targetId = if matchUserId match' == uid' then matchTargetId match'
                                          else matchUserId match'
      (answers1, answers2) <- getLargestIntersectionAnswers conn' uid' targetId
      userAorbInfo <- getUserAorbAndAssoc conn' uid'
      let weights = case userAorbInfo of
            Just (aorb, _) ->
              map (\(i,_) -> if i == aorbId aorb then 5 else 1)
                  (zip [0..] answers1)
            Nothing -> replicate (length answers1) 1
      return (match', weightedYuleQ answers1 answers2 weights)

-- | Response Helpers

redirectToLogin :: Wai.Response
redirectToLogin = Wai.responseLBS
  HTTP.status303
  [ (Headers.hLocation, "/login")
  , (Headers.hContentType, "text/html")
  ]
  ""

invalidSubmissionResponse :: Wai.Response
invalidSubmissionResponse = Wai.responseLBS
  HTTP.status400
  [(Headers.hContentType, BS.pack "text/html")]
  (R.renderHtml invalidSubmissionTemplate)

notFoundResponse :: Wai.Response
notFoundResponse = notFoundTemplateRoute undefined

notFoundTemplateRoute :: Wai.Request -> Wai.Response
notFoundTemplateRoute _ = Wai.responseLBS
  HTTP.status404
  [(Headers.hContentType, BS.pack "text/html")]
  (R.renderHtml notFoundTemplate)

-- | Protected Route Handlers

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
              (R.renderHtml userNotFoundTemplate)
            [user] -> do
              now <- POSIXTime.getPOSIXTime
              let query = SQL.Query $ T.unwords
                    [ "INSERT INTO auth"
                    , "(user_id, hash, created_on, last_accessed)"
                    , "VALUES (?, ?, ?, ?)"
                    ]
              hash <- generateAuthHash (TE.decodeUtf8 email)
              SQL.execute conn query
                (userId user, hash :: T.Text, show now, show now)
              emailConfig <- getEmailConfig
              sendAuthEmail emailConfig (TE.decodeUtf8 email) hash
              return $ Wai.responseLBS
                HTTP.status200
                [(Headers.hContentType, "text/html")]
                (R.renderHtml $ emailSentTemplate)
            _ -> return $ Wai.responseLBS
              HTTP.status500
              [(Headers.hContentType, "text/html")]
              (R.renderHtml internalErrorTemplate)
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
              (R.renderHtml emailExistsTemplate)
            [] -> do
              now <- POSIXTime.getPOSIXTime
              uuid <- UUID.toString <$> UUID.nextRandom
              let newUser = User
                    { userId = 0
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
                    (userId user, hash :: T.Text, show now, show now)
                  emailConfig <- getEmailConfig
                  sendAuthEmail emailConfig (userEmail newUser) hash
                  return $ Wai.responseLBS
                    HTTP.status200
                    [(Headers.hContentType, "text/html")]
                    (R.renderHtml $ emailSentTemplate)
                _ -> return $ Wai.responseLBS
                  HTTP.status500
                  [(Headers.hContentType, "text/html")]
                  (R.renderHtml internalErrorTemplate)
    _ -> return $ Wai.responseLBS
      HTTP.status400
      [(Headers.hContentType, "text/html")]
      (R.renderHtml invalidSubmissionTemplate)

authHashRoute :: SQL.Connection -> T.Text -> Wai.Request -> IO Wai.Response
authHashRoute conn hash _ = do
  maybeUser <- getUserFromAuthHash conn hash
  case maybeUser of
    Nothing -> return $ Wai.responseLBS
      HTTP.status404
      [(Headers.hContentType, "text/html")]
      (R.renderHtml notFoundTemplate)
    Just _ -> do
      now <- POSIXTime.getPOSIXTime
      SQL.execute conn
        "UPDATE auth SET last_accessed = ? WHERE hash = ?" (show now, hash)
      return $ setCookie hash $ Wai.responseLBS
        HTTP.status303
        [ (Headers.hLocation, "/whoami")
        , (Headers.hContentType, "text/html")
        ]
        ""

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
