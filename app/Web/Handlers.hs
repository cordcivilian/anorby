{-# LANGUAGE OverloadedStrings #-}

module Web.Handlers where

import qualified Control.Monad as Monad
import qualified System.Directory as Dir
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Digest.Pure.SHA as SHA
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import qualified Data.Time.Clock.POSIX as POSIXTime
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import qualified Data.Word as Word
import qualified Database.SQLite.Simple as SQL
import qualified Network.HTTP.Types as HTTP
import qualified Network.HTTP.Types.Header as Headers
import qualified Network.Wai as Wai
import qualified Network.Wai.Parse as Wai
import qualified System.Random as Random
import qualified Text.Blaze.Html.Renderer.Utf8 as R
import qualified Text.Read as Read

import Auth
import Core.Matching
import Core.Similarity
import Core.RollingShadow
import Database
import Types
import Utils.Config
import Utils.Cache
import Utils.Email
import Utils.Time
import Utils.MatchState
import Web.Templates
import Web.Types

-- | Public Route Handlers

serveStaticFile :: BS.ByteString -> IO Wai.Response
serveStaticFile path = do
  projectDir <- Dir.getCurrentDirectory
  let filePath = projectDir ++ BS.unpack path
  exists <- Dir.doesFileExist filePath
  if exists
    then do
      content <- BSL.readFile filePath
      return $ Wai.responseLBS
        HTTP.status200
        [(Headers.hContentType, getMimeType path)]
        content
    else do
      return notFoundResponse

serveCachedCss :: AppState -> BS.ByteString -> Wai.Request -> IO Wai.Response
serveCachedCss state path req = do
  cached <- getFromCache (BS.unpack path) (appStaticCache state)
  case cached of
    Just (content, etag) ->
      serveWithEtag req path content etag
    Nothing -> do
      projectDir <- Dir.getCurrentDirectory
      let filePath = projectDir ++ BS.unpack path
      exists <- Dir.doesFileExist filePath
      if exists
        then do
          content <- BSL.readFile filePath
          let etag = T.pack $ SHA.showDigest $ SHA.sha1 content
          putInCacheWithTTL
            (BS.unpack path)
            (content, etag)
            (365 * 24 * 60 * 60)
            (appStaticCache state)
          serveWithEtag req path content etag
        else do
          return notFoundResponse

serveWithEtag :: Wai.Request -> BS.ByteString -> BSL.ByteString -> T.Text -> IO Wai.Response
serveWithEtag req path content etag = do
  let ifNoneMatch = lookup Headers.hIfNoneMatch (Wai.requestHeaders req)
  case ifNoneMatch of
    Just clientEtag | clientEtag == TE.encodeUtf8 etag -> do
      let headers = [ (Headers.hETag, TE.encodeUtf8 etag),
                      (Headers.hCacheControl, "public, max-age=31536000")
                    ]
      return $ Wai.responseLBS HTTP.status304 headers ""
    _ -> do
      let headers = [ (Headers.hContentType, getMimeType path),
                      (Headers.hETag, TE.encodeUtf8 etag),
                      (Headers.hCacheControl, "public, max-age=31536000")
                    ]
      return $ Wai.responseLBS HTTP.status200 headers content

getMimeType :: BS.ByteString -> BS.ByteString
getMimeType path
  | BS.isSuffixOf ".css" path = "text/css"
  | BS.isSuffixOf ".js" path = "application/javascript"
  | otherwise = "application/octet-stream"

getCachedQuery :: AppState -> SQL.Connection -> String -> SQL.Query -> [SQL.SQLData] -> IO Int
getCachedQuery state conn key query params = do
  cached <- getFromCache key (appQueryCache state)
  case cached of
    Just (IntResult i) -> return i
    Just (UserResult _) -> fetchAndCache
    Just (AorbResult _) -> fetchAndCache
    Nothing -> fetchAndCache
  where
    fetchAndCache = do
      [SQL.Only result] <- SQL.query conn query params
      putInCacheWithTTL key (IntResult result) 30 (appQueryCache state)
      return result

getRootStats :: AppState -> SQL.Connection -> IO RootStats
getRootStats state conn = do
  cachedStats <- getFromCache "root_stats" (appStatsCache state)
  case cachedStats of
    Just stats -> return stats
    Nothing -> do
      now <- POSIXTime.getPOSIXTime
      let startOfDay = floor now - (mod (floor now) 86400)
          weekAgo = floor now - (7 * 24 * 60 * 60)

      totalQuestions' <- getCachedQuery state conn "total_questions"
        "SELECT COUNT(*) FROM aorb" []

      totalAnswers' <- getCachedQuery state conn "total_answers"
        "SELECT COUNT(*) FROM aorb_answers WHERE user_id != -1" []

      todayAnswers' <- getCachedQuery state conn "today_answers"
        "SELECT COUNT(*) FROM aorb_answers WHERE user_id != -1 AND answered_on >= ?"
        [SQL.SQLInteger startOfDay]

      activeUsers' <- getCachedQuery state conn "active_users"
        "SELECT COUNT(DISTINCT user_id) FROM aorb_answers WHERE user_id != -1"
        []

      newUsers' <- getCachedQuery state conn "new_users"
        "SELECT COUNT(*) FROM users WHERE created_on >= ? AND id != -1"
        [SQL.SQLInteger weekAgo]

      newQuestions' <- getCachedQuery state conn "new_questions"
        "SELECT COUNT(*) FROM aorb WHERE created_on >= ?"
        [SQL.SQLInteger weekAgo]

      enrolled <- getUsersWithCompletedAnswers conn
      let enrolledCount' = length $ filter (/= shadowUserId) enrolled

      let stats = RootStats
            { rootTotalQuestions = totalQuestions'
            , rootTotalAnswers = totalAnswers'
            , rootTodayAnswers = todayAnswers'
            , rootActiveUsers = activeUsers'
            , rootNewUsers = newUsers'
            , rootNewQuestions = newQuestions'
            , rootEnrolledCount = enrolledCount'
            }

      putInCacheWithTTL "root_stats" stats (5 * 60) (appStatsCache state)
      return stats

rootTemplateRoute :: AppState -> SQL.Connection -> Wai.Request -> IO Wai.Response
rootTemplateRoute state conn _ = do
  cachedHtml <- getFromCache "root_html" (appHtmlCache state)
  case cachedHtml of
    Just html -> return $ Wai.responseLBS
      HTTP.status200
      [(Headers.hContentType, "text/html")]
      html

    Nothing -> do
      cachedAorbs <- getFromCache "root_aorbs" (appRootCache state)
      aorbs <- case cachedAorbs of
        Just as -> return as
        Nothing -> do
          as <- SQL.query_ conn "SELECT * FROM aorb" :: IO [Aorb]
          gen <- Random.getStdGen
          let (shuffledAorbs, _) = fisherYatesShuffle gen as
          putInCacheWithTTL "root_aorbs" shuffledAorbs (60 * 60) (appRootCache state)
          return shuffledAorbs

      stats <- getRootStats state conn
      matchStatus <- getMatchStatus (appMatchState state)

      let
        html =
          R.renderHtml $ rootTemplate
            (rootTotalQuestions stats)
            (rootTotalAnswers stats)
            (rootTodayAnswers stats)
            (rootActiveUsers stats)
            (rootNewUsers stats)
            (rootNewQuestions stats)
            (rootEnrolledCount stats)
            matchStatus
            aorbs

      putInCacheWithTTL "root_html" html (2 * 60) (appHtmlCache state)

      return $ Wai.responseLBS
        HTTP.status200
        [(Headers.hContentType, "text/html")]
        html

adminTemplateRoute :: SQL.Connection -> UserID -> Wai.Request -> IO Wai.Response
adminTemplateRoute conn _ _ = do
  aorbs <- SQL.query_ conn "SELECT * FROM aorb ORDER BY id" :: IO [Aorb]
  return $ Wai.responseLBS
    HTTP.status200
    [(Headers.hContentType, "text/html; charset=utf-8")]
    (R.renderHtml $ adminTemplate aorbs)

handleAddAorb :: Config -> SQL.Connection -> UserID -> Wai.Request -> IO Wai.Response
handleAddAorb _ conn _ req = do
  (params, _) <- Wai.parseRequestBody Wai.lbsBackEnd req
  let getParam name = case lookup name params of
        Just value -> Just $ TE.decodeUtf8With TEE.lenientDecode value
        Nothing -> Nothing
  case (getParam "context", getParam "subtext", getParam "option_a", getParam "option_b") of
    (Just ctx, Just stx, Just a, Just b) -> do
      SQL.execute conn
        "INSERT INTO aorb (context, subtext, a, b) VALUES (?, ?, ?, ?)"
        (ctx, stx, a, b)
      return $ Wai.responseLBS
        HTTP.status303
        [(Headers.hLocation, "/admin")]
        ""
    _ -> return invalidSubmissionResponse

handleEditAorbForm :: Config -> SQL.Connection -> UserID -> AorbID -> Wai.Request -> IO Wai.Response
handleEditAorbForm _ conn _ aid _ = do
  aorbs <- SQL.query conn
    "SELECT * FROM aorb WHERE id = ?"
    (SQL.Only aid) :: IO [Aorb]
  case aorbs of
    [aorb] -> return $ Wai.responseLBS
      HTTP.status200
      [(Headers.hContentType, "text/html; charset=utf-8")]
      (R.renderHtml $ editAorbTemplate aorb)
    _ -> return notFoundResponse

handleEditAorb :: Config -> SQL.Connection -> UserID -> AorbID -> Wai.Request -> IO Wai.Response
handleEditAorb _ conn _ aid req = do
  (params, _) <- Wai.parseRequestBody Wai.lbsBackEnd req
  let getParam name = case lookup name params of
        Just value -> Just $ TE.decodeUtf8With TEE.lenientDecode value
        Nothing -> Nothing
  case (getParam "context", getParam "subtext", getParam "option_a", getParam "option_b") of
    (Just ctx, Just stx, Just a, Just b) -> do
      SQL.execute conn
        "UPDATE aorb SET context = ?, subtext = ?, a = ?, b = ? WHERE id = ?"
        (ctx, stx, a, b, aid)
      return $ Wai.responseLBS
        HTTP.status303
        [(Headers.hLocation, "/admin")]
        ""
    _ -> return invalidSubmissionResponse

handleDeleteAorb :: Config -> SQL.Connection -> UserID -> AorbID -> Wai.Request -> IO Wai.Response
handleDeleteAorb _ conn _ aid _ = do
  exists <- SQL.query conn
    "SELECT 1 FROM aorb WHERE id = ? LIMIT 1"
    (SQL.Only aid) :: IO [SQL.Only Int]
  case exists of
    [] -> return notFoundResponse
    _ -> do
      SQL.execute conn
        "DELETE FROM aorb_answers WHERE aorb_id = ?"
        (SQL.Only aid)
      SQL.execute conn
        "DELETE FROM aorb WHERE id = ?"
        (SQL.Only aid)
      return $ Wai.responseLBS
        HTTP.status303
        [ (Headers.hLocation, "/admin")
        , (Headers.hContentType, "text/html")
        ]
        ""

profileTemplateRoute :: Config -> SQL.Connection -> UserID -> Wai.Request -> IO Wai.Response
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
          let shareBaseUrl =
                if environment config == Production
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

sharedProfileTemplateRoute :: SQL.Connection -> T.Text -> Wai.Request -> IO Wai.Response
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

answerTemplateRoute :: SQL.Connection -> UserID -> Wai.Request -> IO Wai.Response
answerTemplateRoute conn uid _ = do
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
            (R.renderHtml $ answerTemplate aorb shouldSwap token)

existingAnswerTemplateRoute :: SQL.Connection -> UserID -> AorbID -> Wai.Request -> IO Wai.Response
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

submitAnswerRoute :: SQL.Connection -> UserID -> AorbID -> AorbAnswer -> T.Text -> Wai.Request -> IO Wai.Response
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
              SQL.execute conn
                (SQL.Query $ T.unwords
                  [ "INSERT INTO aorb_answers"
                  , "(user_id, aorb_id, answer)"
                  , "VALUES (?, ?, ?)"
                  ])
                (uid, aid, answer)
              return $ Wai.responseLBS
                HTTP.status303
                [ (Headers.hLocation, BS.pack "/ans")
                , (Headers.hContentType, BS.pack "text/html")
                ]
                ""

editAnswerRoute :: SQL.Connection -> UserID -> AorbID -> AorbAnswer -> T.Text -> Wai.Request -> IO Wai.Response
editAnswerRoute conn uid aid answer token _ = do
  isValid <- validateAnswerToken token uid aid
  if not isValid
    then return $ Wai.responseLBS
      HTTP.status403
      [(Headers.hContentType, BS.pack "text/html")]
      (R.renderHtml invalidTokenTemplate)
    else do
      SQL.execute conn
        (SQL.Query $ T.unwords
          [ "UPDATE aorb_answers"
          , "SET answer = ?"
          , "WHERE user_id = ? AND aorb_id = ?"
          ])
        (answer, uid, aid)
      return $ Wai.responseLBS
        HTTP.status303
        [ (Headers.hLocation, BS.pack $ "/ans/" ++ show aid)
        , (Headers.hContentType, BS.pack "text/html")
        ]
        ""

setFavoriteAorbRoute :: SQL.Connection -> UserID -> AorbID -> Wai.Request -> IO Wai.Response
setFavoriteAorbRoute conn uid aid _ = do
  SQL.execute conn "UPDATE users SET aorb_id = ? WHERE id = ?" (aid, uid)
  return $ Wai.responseLBS
    HTTP.status303
    [ (Headers.hLocation, BS.pack $ "/ans/" ++ show aid)
    , (Headers.hContentType, BS.pack "text/html")
    ]
    ""

matchTemplateRoute :: Config -> AppState -> SQL.Connection -> UserID -> Wai.Request -> IO Wai.Response
matchTemplateRoute config state conn uid _ = do
  hasAccess <- hasThresholdAccess conn uid (matchThreshold config)
  if not hasAccess
    then return $ Wai.responseLBS
      HTTP.status200
      [(Headers.hContentType, BS.pack "text/html")]
      (R.renderHtml $ matchNotYetActive (matchThreshold config))
    else do
      userQ <- SQL.query conn "SELECT * FROM users WHERE id = ?" (SQL.Only uid)
      case userQ of
        (user:_) -> do
          now <- POSIXTime.getPOSIXTime
          enrolled <- getUsersWithCompletedAnswers conn
          maybeCutoffTime <- parseMatchTime (matchCutoffTime config)
          maybeReleaseTime <- parseMatchTime (matchReleaseTime config)
          matchStatus <- getMatchStatus (appMatchState state)

          let startOfDay = (floor (now / 86400) * 86400) :: Integer
              endOfDay = startOfDay + 86400
          todayMatches <- SQL.query conn
            (SQL.Query $ T.unwords
              [ "SELECT user_id, target_id, matched_on"
              , "FROM matched"
              , "WHERE (user_id = ? OR target_id = ?)"
              , "AND matched_on >= ? AND matched_on < ?"
              , "ORDER BY matched_on DESC LIMIT 1"
              ])
            (uid, uid, startOfDay, endOfDay) :: IO [Match]

          todayMatchScore <- case todayMatches of
            (match:_) -> do
              (_, score) <- calculateMatchScore conn uid match
              return $ Just (match, score)
            [] -> return Nothing

          matches <- getUserMatches conn uid
          let expiryAgo = floor now - (matchExpiryDays config * 24 * 60 * 60)
              groupedMatches = groupMatchesByDay matches
              uniqueDayMatches =
                reverse $ Maybe.mapMaybe (Maybe.listToMaybe . snd) groupedMatches
              pastMatches = filter (\m -> matchTimestamp m < startOfDay &&
                                        matchTimestamp m >= expiryAgo) uniqueDayMatches

          pastMatchesData <- mapM (\m -> do
            (_, score) <- calculateMatchScore conn uid m
            unreadCount <- getUnreadMessageCount conn uid m
            return (m, score, unreadCount)) pastMatches

          return $ Wai.responseLBS
            HTTP.status200
            [(Headers.hContentType, BS.pack "text/html")]
            (R.renderHtml $
              matchTemplate
                config user (elem uid enrolled) (length $ filter (/= shadowUserId) enrolled)
                maybeCutoffTime maybeReleaseTime now
                todayMatchScore matchStatus pastMatchesData
            )
        [] -> return notFoundResponse

handleMatchTypeUpdate :: SQL.Connection -> UserID -> Wai.Request -> IO Wai.Response
handleMatchTypeUpdate conn uid req = do
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
            [ (Headers.hLocation, "/clash")
            , (Headers.hContentType, "text/html")
            ]
            ""
        Nothing -> return invalidSubmissionResponse
    Nothing -> return invalidSubmissionResponse
  where
    parseAssociationScheme :: T.Text -> Maybe AssociationScheme
    parseAssociationScheme "PPPod" = Just PPPod
    parseAssociationScheme "Swing" = Just Swing
    parseAssociationScheme "Bipolar" = Just Bipolar
    parseAssociationScheme _ = Nothing

calculateMatchScore :: SQL.Connection -> UserID -> Match -> IO (Match, Double)
calculateMatchScore conn uid match = do
  let targetId = if matchUserId match == uid
                 then matchTargetId match
                 else matchUserId match
  (answers1, answers2) <- getLargestIntersectionAnswers conn uid targetId
  userAorbInfo <- getUserAorbAndAssoc conn uid
  let weights = case userAorbInfo of
        Just (aorb, _) ->
          map (\(i,_) -> if i == aorbId aorb then 5 else 1)
              (zip [0..] answers1)
        Nothing -> replicate (length answers1) 1
  return (match, weightedYuleQ answers1 answers2 weights)

groupMatchesByDay :: [Match] -> [(Integer, [Match])]
groupMatchesByDay ms =
  Map.toList $ foldr addToDay Map.empty ms
  where
    addToDay m acc =
      let dayTimestamp = (div (matchTimestamp m) 86400) * 86400
      in Map.insertWith (++) dayTimestamp [m] acc

matchProfileTemplateRoute :: Config -> SQL.Connection -> UserID -> Integer -> Wai.Request -> IO Wai.Response
matchProfileTemplateRoute config conn uid days _ = do
  now <- POSIXTime.getPOSIXTime
  let startOfDay = floor (now / 86400) * 86400
      targetDay = startOfDay - (days * 86400)
      nextDay = targetDay + 86400
  matches <- SQL.query conn
    (SQL.Query $ T.unwords
      [ "SELECT id, user_id, target_id, matched_on"
      , "FROM matched"
      , "WHERE (user_id = ? OR target_id = ?)"
      , "AND matched_on >= ? AND matched_on < ?"
      , "ORDER BY matched_on DESC LIMIT 1"
      ])
    (uid, uid, targetDay, nextDay) :: IO [MatchRecord]
  case matches of
    (MatchRecord (matchId, match):_) -> do
      let targetId = if matchUserId match == uid
                    then matchTargetId match
                    else matchUserId match
      (answers1, answers2) <- getLargestIntersectionAnswers conn uid targetId
      userAorbInfo <- getUserAorbAndAssoc conn uid
      let weights = case userAorbInfo of
            Just (aorb, _) ->
              map (\(i,_) -> if i == aorbId aorb then 5 else 1)
                  (zip [0..] answers1)
            Nothing -> replicate (length answers1) 1
      let agreementRate = (weightedYuleQ answers1 answers2 weights + 1) * 50
      yourTotalAnswers <- getUserTotalAnswerCount conn uid
      targetTotalAnswers <- getUserTotalAnswerCount conn targetId
      targetMainAorbs <- getMatchesMainAorbs conn uid targetId
      agreements <- getMatchesTopXUniqueAgreement conn uid targetId 1
      disagreements <- getMatchesTopXCommonDisagreement conn uid targetId 1

      guessResults <- getGuessResults conn matchId uid targetId
      guessAorbs <- if length guessResults < 3
                       then do
                         let neededGuesses = 3 - length guessResults
                         let guessedAorbIds = map (aorbId . guessResultAorb) guessResults
                         let shownAorbIds =
                               (maybe [] (\(a1, a2) -> [aorbId (matchingAorbData a1), aorbId (matchingAorbData a2)]) targetMainAorbs) ++
                               (maybe [] (\a -> [aorbId (matchingAorbData a)]) $ Maybe.listToMaybe agreements) ++
                               (maybe [] (\a -> [aorbId (matchingAorbData a)]) $ Maybe.listToMaybe disagreements)
                         let excludeIds = shownAorbIds ++ guessedAorbIds
                         aorbs <- getGuessAorbs conn targetId excludeIds neededGuesses
                         return aorbs
                       else
                         return []

      stereoGuesses <- getStereoGuesses conn matchId uid

      let hasCorrectGuess = any guessResultCorrect guessResults
          existingStereoIds = map stereoGuessStereoId stereoGuesses

      stereoQuestions <- if hasCorrectGuess && length stereoGuesses < 3 then getStereoQuestionsExcluding conn existingStereoIds (3 - length stereoGuesses) else return []
      stereoGuessesAboutUser <- getStereoGuessesForTarget conn matchId uid

      let allStereoIds = List.nub $ map stereoGuessStereoId stereoGuesses ++ map stereoGuessStereoId stereoGuessesAboutUser ++ map stereoId stereoQuestions

      allStereoQuestions <- Monad.forM allStereoIds $ \sid -> do
        maybeStereo <- getStereoById conn sid
        return $ maybe Nothing (\s -> Just (sid, s)) maybeStereo

      let stereoMap = Map.fromList $ Maybe.catMaybes allStereoQuestions

      let matchView = MatchView
            { viewTimestamp = matchTimestamp match
            , viewAgreementRate = agreementRate
            , viewTargetTotalAnswers = targetTotalAnswers
            , viewYourTotalAnswers = yourTotalAnswers
            , viewMainAorbs = targetMainAorbs
            , viewTopAgreement = Maybe.listToMaybe agreements
            , viewTopDisagreement = Maybe.listToMaybe disagreements
            , viewGuessAorbs = guessAorbs
            , viewGuessResults = guessResults
            , viewStereoQuestions = stereoQuestions
            , viewStereoGuesses = stereoGuesses
            }

      messages <- getMessagesForMatch conn matchId
      markMessagesRead conn matchId uid

      return $ Wai.responseLBS
        HTTP.status200
        [ (Headers.hContentType, "text/html; charset=utf-8") ]
        (R.renderHtml $ matchProfileTemplate config days uid targetId matchId matchView messages stereoGuessesAboutUser stereoMap)
    [] -> return notFoundResponse

handleGuessSubmission :: Config -> SQL.Connection -> UserID -> Integer -> Wai.Request -> IO Wai.Response
handleGuessSubmission _ conn uid days req = do
  (params, _) <- Wai.parseRequestBody Wai.lbsBackEnd req
  case (,,) <$> lookup "aorb_id" params <*> lookup "choice" params <*> lookup "match_id" params of
    Just (aorbIdBS, choiceBS, matchIdBS) -> do
      case ( Read.readMaybe (BS.unpack aorbIdBS) :: Maybe AorbID
           , Read.readMaybe (BS.unpack choiceBS) :: Maybe Word.Word8
           , Read.readMaybe (BS.unpack matchIdBS) :: Maybe Int
           ) of
        (Just aid, Just choice, Just matchId) -> do
          saveGuess conn matchId uid aid (AorbAnswer choice)
          return $ Wai.responseLBS
            HTTP.status303
            [ (Headers.hLocation, BS.pack $ "/clash/t-" ++ show days ++ "#guesses") ]
            ""
        _ -> do
          return invalidSubmissionResponse
    Nothing -> do
      return invalidSubmissionResponse

submitGuessRoute :: Config -> SQL.Connection -> UserID -> Integer -> AorbID -> AorbAnswer -> Wai.Request -> IO Wai.Response
submitGuessRoute _ conn uid days aid answer _ = do
  now <- POSIXTime.getPOSIXTime
  let startOfDay = floor (now / 86400) * 86400
      targetDay = startOfDay - (days * 86400)
      nextDay = targetDay + 86400

  matches <- SQL.query conn
    (SQL.Query $ T.unwords
      [ "SELECT id"
      , "FROM matched"
      , "WHERE (user_id = ? OR target_id = ?)"
      , "AND matched_on >= ? AND matched_on < ?"
      , "ORDER BY matched_on DESC LIMIT 1"
      ])
    (uid, uid, targetDay, nextDay) :: IO [SQL.Only Int]

  case matches of
    [SQL.Only matchId] -> do
      saveGuess conn matchId uid aid answer
      return $ Wai.responseLBS
        HTTP.status303
        [ (Headers.hLocation,
           BS.pack $ "/clash/t-" ++ show days ++ "#guesses")
        ]
        ""
    _ -> return notFoundResponse

handleStereoSubmission :: Config -> SQL.Connection -> UserID -> Integer -> Wai.Request -> IO Wai.Response
handleStereoSubmission _ conn uid days req = do
  (params, _) <- Wai.parseRequestBody Wai.lbsBackEnd req
  case (,,,) <$> lookup "stereo_id" params <*> lookup "choice" params <*> lookup "match_id" params <*> lookup "target_id" params of
    Just (stereoIdBS, choiceBS, matchIdBS, targetIdBS) -> do
      case ( Read.readMaybe (BS.unpack stereoIdBS) :: Maybe Int
           , Read.readMaybe (BS.unpack choiceBS) :: Maybe Word.Word8
           , Read.readMaybe (BS.unpack matchIdBS) :: Maybe Int
           , Read.readMaybe (BS.unpack targetIdBS) :: Maybe UserID
           ) of
        (Just sid, Just choice, Just matchId, Just targetId) -> do
          matchDetails <- SQL.query conn
            "SELECT 1 FROM matched WHERE id = ? AND (user_id = ? OR target_id = ?)"
            (matchId, uid, uid) :: IO [SQL.Only Int]

          if null matchDetails
            then return notFoundResponse
            else do

              guessResults <- getGuessResults conn matchId uid targetId
              let hasCorrectGuess = any guessResultCorrect guessResults
                  hasCompletedAllBaseGuesses = length guessResults >= 3

              if not (hasCorrectGuess && hasCompletedAllBaseGuesses)
                then return $ Wai.responseLBS
                  HTTP.status403
                  [(Headers.hContentType, BS.pack "text/html")]
                  (R.renderHtml $ errorTemplateWithLink 403 "must complete all 3 base guesses with at least one correct" ("/clash/t-" <> T.pack (show days), "back"))
                else do
                  saveStereoGuess conn matchId uid targetId sid (AorbAnswer choice)
                  return $ Wai.responseLBS
                    HTTP.status303
                    [ (Headers.hLocation, BS.pack $ "/clash/t-" ++ show days ++ "#stereo") ]
                    ""
        _ -> do
          return invalidSubmissionResponse
    Nothing -> do
      return invalidSubmissionResponse

postMessageRoute :: Config -> SQL.Connection -> UserID -> Integer -> Wai.Request -> IO Wai.Response
postMessageRoute config conn uid days req = do
  (params, _) <- Wai.parseRequestBody Wai.lbsBackEnd req
  case lookup "new-message" params of
    Nothing -> return invalidSubmissionResponse
    Just contentBS -> do
      let content = TE.decodeUtf8With TEE.lenientDecode contentBS
      now <- POSIXTime.getPOSIXTime
      let startOfDay = floor (now / 86400) * 86400
          targetDay = startOfDay - (days * 86400)
          nextDay = targetDay + 86400
      matches <- SQL.query conn
        (SQL.Query $ T.unwords
          [ "SELECT id"
          , "FROM matched"
          , "WHERE (user_id = ? OR target_id = ?)"
          , "AND matched_on >= ? AND matched_on < ?"
          , "ORDER BY matched_on DESC LIMIT 1"
          ])
        (uid, uid, targetDay, nextDay) :: IO [SQL.Only Int]
      case matches of
        [SQL.Only matchId] -> do
          isValid <- validateNewMessage config conn matchId uid content
          if isValid
            then do
              insertMessage conn matchId uid content
              return $ Wai.responseLBS
                HTTP.status303
                [ (Headers.hLocation,
                   BS.pack $ "/clash/t-" ++ show days ++ "#bottom")
                ]
                ""
            else return invalidSubmissionResponse
        _ -> return notFoundResponse

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
              let query = SQL.Query $ T.unwords
                    [ "INSERT INTO auth"
                    , "(user_id, hash)"
                    , "VALUES (?, ?)"
                    ]
              hash <- generateAuthHash (TE.decodeUtf8 email)
              SQL.execute conn query
                (userId user, hash :: T.Text)
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
                    , userCreatedOn = 0
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
                    , floor now :: Integer
                    , floor now :: Integer
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
      SQL.execute conn
        "UPDATE auth SET last_accessed = unixepoch('now') WHERE hash = ?"
        (SQL.Only hash)
      return $ setCookie hash $ Wai.responseLBS
        HTTP.status303
        [ (Headers.hLocation, "/whoami")
        , (Headers.hContentType, "text/html")
        ]
        ""

accountTemplateRoute :: SQL.Connection -> UserID -> Wai.Request -> IO Wai.Response
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

logoutConfirmRoute :: SQL.Connection -> UserID -> Wai.Request -> IO Wai.Response
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

logoutConfirmPostRoute :: SQL.Connection -> UserID -> Wai.Request -> IO Wai.Response
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

deleteConfirmRoute :: SQL.Connection -> UserID -> Wai.Request -> IO Wai.Response
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

deleteConfirmPostRoute :: SQL.Connection -> UserID -> Wai.Request -> IO Wai.Response
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
