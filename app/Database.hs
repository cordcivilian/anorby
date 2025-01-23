{-# LANGUAGE OverloadedStrings #-}

module Database
  ( initDB
  , initConn
  , initPool
  , initTables
  , readBaseAorbs
  , ingestBaseAorbData
  , getUsersWithAnswers
  , getUsersAnswers
  , getUsersAorbIdAndAssoc
  , baseAorbsToSubmissions
  , allAorbsToSubmissions
  , getUserByUuid
  , getUserUuidById
  , getUserTotalAnswerCount
  , getUserFromAuthHash
  , getUsersWithCompletedAnswers
  , getNextUnansweredAorb
  , getDailyAnswerCount
  , getUserAorbAndAssoc
  , getUserTopXMostCommonplace
  , getUserTopXMostControversial
  , getUserAorbsFromControversialToCommonPlace
  , getMatchesMainAorbs
  , getMatchesTopXCommonDisagreement
  , getMatchesTopXUniqueAgreement
  , getLargestIntersection
  , getLargestIntersectionAnswers
  ) where

import qualified Control.Monad as Monad
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy as BSL
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Pool as Pool
import qualified Data.Text as T
import qualified Data.Time as Time
import qualified Database.SQLite.Simple as SQL

import Types

-- | Database initialization and connection management

initDB :: FilePath -> Bool -> IO SQL.Connection
initDB db clean = do
  conn <- SQL.open db
  Monad.when clean $ do
    putStrLn "Cleaning existing database..."
    let cleanupQueries =
          [ "PRAGMA writable_schema = 1"
          , "DELETE FROM sqlite_master"
          , "PRAGMA writable_schema = 0"
          , "VACUUM"
          , "PRAGMA integrity_check"
          ]
    mapM_ (SQL.execute_ conn . SQL.Query . T.pack) cleanupQueries
    initTables conn
    ingestBaseAorbData conn
  initSQLitePragmas conn
  return conn

initConn :: FilePath -> IO SQL.Connection
initConn db = SQL.open db

initPool :: FilePath -> IO (Pool.Pool SQL.Connection)
initPool db = Pool.newPool $
  Pool.defaultPoolConfig (initConn db) SQL.close 30 10

-- | SQLite optimization settings
initSQLitePragmas :: SQL.Connection -> IO ()
initSQLitePragmas conn = do
  putStrLn "Initializing SQLite pragmas for performance optimization..."
  let pragmaSettings =
        [ "PRAGMA journal_mode = WAL"
        , "PRAGMA synchronous = NORMAL"
        , "PRAGMA busy_timeout = 5000"
        , "PRAGMA cache_size = -20000"
        , "PRAGMA foreign_keys = ON"
        , "PRAGMA auto_vacuum = INCREMENTAL"
        , "PRAGMA temp_store = MEMORY"
        , "PRAGMA mmap_size = 2147483648"
        , "PRAGMA page_size = 8192"
        ]
  mapM_ (SQL.execute_ conn) pragmaSettings

  putStrLn "Verifying SQLite settings:"
  let pragmas =
        [ "journal_mode"     -- TEXT
        , "synchronous"      -- INTEGER
        , "busy_timeout"     -- INTEGER
        , "cache_size"       -- INTEGER
        , "foreign_keys"     -- INTEGER
        , "auto_vacuum"      -- INTEGER
        , "temp_store"       -- INTEGER
        , "mmap_size"        -- INTEGER
        , "page_size"        -- INTEGER
        ]
  Monad.forM_ pragmas $ \pragma -> do
    let query = SQL.Query $ "PRAGMA " <> T.pack pragma
    case pragma of
      "journal_mode" -> do
        [SQL.Only value] <- SQL.query_ conn query :: IO [SQL.Only T.Text]
        putStrLn $ pragma <> " = " <> T.unpack value
      _ -> do
        [SQL.Only value] <- SQL.query_ conn query :: IO [SQL.Only Int]
        putStrLn $ pragma <> " = " <> show value

-- | Table initialization

initTables :: SQL.Connection -> IO ()
initTables conn = SQL.withTransaction conn $ do
  initUserTable conn
  initAorbTable conn
  initAorbAnswersTable conn
  initAorbMeanTrigger conn
  initAuthTable conn

initAuthTable :: SQL.Connection -> IO ()
initAuthTable conn = SQL.execute_ conn $ SQL.Query $ T.unwords
  [ "CREATE TABLE IF NOT EXISTS auth"
  , "(id INTEGER PRIMARY KEY,"
  , "user_id INTEGER NOT NULL,"
  , "hash TEXT NOT NULL,"
  , "created_on TEXT NOT NULL,"
  , "last_accessed TEXT NOT NULL,"
  , "FOREIGN KEY (user_id) REFERENCES users(id),"
  , "UNIQUE(hash))"
  ]

initUserTable :: SQL.Connection -> IO()
initUserTable conn = SQL.execute_ conn $ SQL.Query $ T.unwords
 [ "CREATE TABLE IF NOT EXISTS users"
 , "(id INTEGER PRIMARY KEY,"
 , "name TEXT NOT NULL,"
 , "email TEXT NOT NULL,"
 , "uuid TEXT NOT NULL,"
 , "aorb_id INTEGER,"
 , "assoc INTEGER,"
 , "UNIQUE(email),"
 , "UNIQUE(uuid))"
 ]

initAorbTable :: SQL.Connection -> IO()
initAorbTable conn = SQL.execute_ conn $ SQL.Query $ T.unwords
 [ "CREATE TABLE IF NOT EXISTS aorb"
 , "(id INTEGER PRIMARY KEY,"
 , "context TEXT NOT NULL,"
 , "subtext TEXT NOT NULL,"
 , "a TEXT NOT NULL,"
 , "b TEXT NOT NULL,"
 , "mean REAL NOT NULL DEFAULT 0.500000000000000)"
 ]

initAorbAnswersTable :: SQL.Connection -> IO()
initAorbAnswersTable conn = SQL.execute_ conn $ SQL.Query $ T.unwords
 [ "CREATE TABLE IF NOT EXISTS aorb_answers"
 , "(user_id INTEGER NOT NULL,"
 , "aorb_id INTEGER NOT NULL,"
 , "answer INTEGER NOT NULL,"
 , "answered_on TEXT NOT NULL,"
 , "FOREIGN KEY (user_id) REFERENCES users(id),"
 , "FOREIGN KEY (aorb_id) REFERENCES aorb(id),"
 , "UNIQUE(user_id, aorb_id))"
 ]

initAorbMeanTrigger :: SQL.Connection -> IO ()
initAorbMeanTrigger conn = do
  SQL.execute_ conn $ SQL.Query $ T.unwords
    [ "CREATE TRIGGER IF NOT EXISTS update_aorb_mean_insert"
    , "AFTER INSERT ON aorb_answers BEGIN"
    , "  UPDATE aorb"
    , "  SET mean = (SELECT AVG(answer)"
    , "              FROM aorb_answers"
    , "              WHERE aorb_id = NEW.aorb_id)"
    , "  WHERE id = NEW.aorb_id;"
    , "END"
    ]
  SQL.execute_ conn $ SQL.Query $ T.unwords
    [ "CREATE TRIGGER IF NOT EXISTS update_aorb_mean_update"
    , "AFTER UPDATE ON aorb_answers BEGIN"
    , "  UPDATE aorb"
    , "  SET mean = (SELECT AVG(answer)"
    , "              FROM aorb_answers"
    , "              WHERE aorb_id = NEW.aorb_id)"
    , "  WHERE id = NEW.aorb_id;"
    , "END"
    ]
  SQL.execute_ conn $ SQL.Query $ T.unwords
    [ "CREATE TRIGGER IF NOT EXISTS update_aorb_mean_delete"
    , "AFTER DELETE ON aorb_answers BEGIN"
    , "  UPDATE aorb"
    , "  SET mean = COALESCE((SELECT AVG(answer)"
    , "                       FROM aorb_answers"
    , "                       WHERE aorb_id = OLD.aorb_id),"
    , "                      0.500000000000000)"
    , "  WHERE id = OLD.aorb_id;"
    , "END"
    ]

-- | Base data management

readBaseAorbs :: IO (Maybe [Aorb])
readBaseAorbs = do
  jsonData <- BSL.readFile "data/base.json"
  return $ JSON.decode jsonData

ingestBaseAorbData :: SQL.Connection -> IO ()
ingestBaseAorbData conn = do
  base <- readBaseAorbs
  case base of
    Just aorbs -> SQL.withTransaction conn $ SQL.executeMany conn
      (SQL.Query $ T.unwords
            [ "INSERT OR REPLACE INTO aorb (id, context, subtext, a, b)"
            , "VALUES (?, ?, ?, ?, ?)"
            ]
      )
      (map (\aorb ->
        (aorbId aorb, aorbCtx aorb, aorbStx aorb, aorbA aorb, aorbB aorb)
           ) aorbs)
    Nothing -> putStrLn "error parsing json"

-- | User query functions

getUserByUuid :: SQL.Connection -> T.Text -> IO (Maybe User)
getUserByUuid conn uuid = do
  users <- SQL.query
      conn "SELECT * FROM users WHERE uuid = ?" (SQL.Only uuid) :: IO [User]
  return $ case users of
    [user] -> Just user
    _ -> Nothing

getUserUuidById :: SQL.Connection -> UserID -> IO (Maybe T.Text)
getUserUuidById conn uid = do
  let query = "SELECT uuid FROM users WHERE id = ?"
  results <- SQL.query conn query (SQL.Only uid) :: IO [SQL.Only T.Text]
  return $ case results of
    [SQL.Only uuid] -> Just uuid
    _ -> Nothing

getUserTotalAnswerCount :: SQL.Connection -> UserID -> IO Int
getUserTotalAnswerCount conn uid = do
  let query = SQL.Query $ T.unwords
        [ "SELECT COUNT(*)"
        , "FROM aorb_answers"
        , "WHERE user_id = ?"
        ]
  counts <- SQL.query conn query [uid] :: IO [SQL.Only Int]
  case counts of
    (count:_) -> return $ SQL.fromOnly count
    [] -> return 0

getUserFromAuthHash :: SQL.Connection -> T.Text -> IO (Maybe User)
getUserFromAuthHash conn hash = do
  let query = SQL.Query $ T.unwords
        [ "SELECT users.*"
        , "FROM users"
        , "JOIN auth ON users.id = auth.user_id"
        , "WHERE auth.hash = ?"
        ]
  now <- Time.getCurrentTime
  results <- SQL.query conn query (SQL.Only hash) :: IO [User]
  case results of
    [user] -> do
      SQL.execute conn
        "UPDATE auth SET last_accessed = ? WHERE hash = ?"
        (now, hash)
      return $ Just user
    _ -> return Nothing

-- | Aggregate users query functions

getUsersWithCompletedAnswers :: SQL.Connection -> IO [UserID]
getUsersWithCompletedAnswers conn = do

  dailyLimitUsers <- SQL.query_ conn $ SQL.Query $ T.unwords
    [ "SELECT DISTINCT user_id"
    , "FROM aorb_answers"
    , "WHERE date(substr(answered_on, 1, 10)) = date('now')"
    , "GROUP BY user_id"
    , "HAVING COUNT(*) >= 10"
    ]

  allQuestionsUsers <- SQL.query_ conn $ SQL.Query $ T.unwords
    [ "SELECT DISTINCT aa.user_id"
    , "FROM aorb_answers aa"
    , "GROUP BY aa.user_id"
    , "HAVING COUNT(DISTINCT aa.aorb_id) = ("
    , "  SELECT COUNT(*)"
    , "  FROM aorb"
    , ")"
    ]

  let dailyLimitIds =
        map SQL.fromOnly (dailyLimitUsers :: [SQL.Only UserID])
      allQuestionsIds =
        map SQL.fromOnly (allQuestionsUsers :: [SQL.Only UserID])

  return $ List.nub $ dailyLimitIds ++ allQuestionsIds

getUsersWithAnswers :: SQL.Connection -> [AorbID] -> IO [UserID]
getUsersWithAnswers conn aorbIds = do
  let placeholders = replicate (length aorbIds) "?"
      inClause = T.intercalate "," placeholders
      query = SQL.Query $ T.unwords
        [ "SELECT user_id"
        , "FROM aorb_answers"
        , "WHERE aorb_id IN (" <> inClause <> ")"
        , "GROUP BY user_id"
        , "HAVING COUNT(DISTINCT aorb_id) = ?"
        ]
      params = aorbIds ++ [length aorbIds]
  users <- SQL.query conn query params :: IO [SQL.Only UserID]
  return $ map SQL.fromOnly users

getUsersAnswers :: SQL.Connection -> [UserID] -> [AorbID]
                -> IO (Map.Map UserID [AorbAnswers])
getUsersAnswers conn users aorbIds = do
  let query = SQL.Query $ T.unwords
        [ "WITH ordered_aorbs AS ("
        , "  SELECT aorb_id, row_number() OVER () as ord"
        , "  FROM (VALUES "
          <> T.intercalate "," (map (\_ -> "(?)") aorbIds) <>
          ") as t(aorb_id)"
        , "),"
        , "user_answers AS ("
        , "  SELECT"
        , "    u.id as user_id,"
        , "    oa.aorb_id,"
        , "    COALESCE(aa.answer, 255) as answer,"
        , "    COALESCE(aa.answered_on, '1970-01-01') as answered_on"
        , "  FROM (VALUES "
          <> T.intercalate "," (replicate (length users) "?") <>
          ") as u(id)"
        , "  CROSS JOIN ordered_aorbs oa"
        , "  LEFT JOIN aorb_answers aa ON"
        , "    aa.user_id = u.id AND aa.aorb_id = oa.aorb_id"
        , ")"
        , "SELECT user_id, aorb_id, answer, answered_on"
        , "FROM user_answers"
        , "ORDER BY user_id, ord"
        ]
  answers <- SQL.query conn query (users ++ aorbIds) :: IO [AorbAnswers]
  return $ Map.fromListWith (++) $
    map (\ans -> (aorbUserId ans, [ans])) answers

getUsersAorbIdAndAssoc :: SQL.Connection -> [UserID]
                       -> IO (Map.Map UserID (AorbID, AssociationScheme))
getUsersAorbIdAndAssoc conn users = do
  let placeholders = replicate (length users) "?"
      inClause = T.intercalate "," placeholders
      query = SQL.Query $ T.unwords
        [ "SELECT id, aorb_id, assoc"
        , "FROM users"
        , "WHERE id IN (" <> inClause <> ")"
        ]
  aorbs <- SQL.query
    conn query users :: IO [(UserID, AorbID, AssociationScheme)]
  return $ Map.fromList [(uid, (aid, assoc)) | (uid, aid, assoc) <- aorbs]

-- | Submissions functions

baseAorbsToSubmissions :: SQL.Connection -> IO Submissions
baseAorbsToSubmissions conn = do
  basedUsers <- getUsersWithAnswers conn [1..100]
  basedUsersAnswers <- getUsersAnswers conn basedUsers [1..100]
  basedUsersAorbIdAssoc <- getUsersAorbIdAndAssoc conn basedUsers
  return $ Map.mapWithKey (\uid (aid, assoc) ->
    let answers =
          maybe [] (map aorbAnswer) $ Map.lookup uid basedUsersAnswers
    in (answers, aid, assoc)
    ) basedUsersAorbIdAssoc

allAorbsToSubmissions :: SQL.Connection -> [UserID] -> IO Submissions
allAorbsToSubmissions conn users = do
  aorbIds <- SQL.query_ conn
    "SELECT id FROM aorb ORDER BY id" :: IO [SQL.Only AorbID]
  let aorbIdList = map SQL.fromOnly aorbIds
  usersAnswers <- getUsersAnswers conn users aorbIdList
  usersAorbIdAssoc <- getUsersAorbIdAndAssoc conn users
  return $ Map.mapWithKey (\uid (aid, assoc) ->
    let answers = maybe [] (map aorbAnswer) $ Map.lookup uid usersAnswers
    in (answers, aid, assoc)
    ) usersAorbIdAssoc

-- | Aorb query functions

getNextUnansweredAorb :: SQL.Connection -> UserID -> IO (Maybe Aorb)
getNextUnansweredAorb conn uid = do
  baseAorb <- getUnansweredAorbWhere conn uid "a.id <= 100"
  case baseAorb of
    Just aorb -> return $ Just aorb
    Nothing -> getUnansweredAorbWhere conn uid "a.id > 100"
  where
    getUnansweredAorbWhere :: SQL.Connection -> UserID -> T.Text
                           -> IO (Maybe Aorb)
    getUnansweredAorbWhere c u condition = do
      let query = SQL.Query $ T.unwords
            [ "SELECT a.*"
            , "FROM aorb a"
            , "WHERE " <> condition
            , "AND NOT EXISTS ("
            , "  SELECT 1"
            , "  FROM aorb_answers ans"
            , "  WHERE ans.aorb_id = a.id"
            , "  AND ans.user_id = ?"
            , ")"
            , "ORDER BY RANDOM()"
            , "LIMIT 1"
            ]
      results <- SQL.query c query [u] :: IO [Aorb]
      return $ case results of
        (x:_) -> Just x
        [] -> Nothing

getDailyAnswerCount :: SQL.Connection -> UserID -> IO Int
getDailyAnswerCount conn uid = do
  let query = SQL.Query $ T.unwords
        [ "SELECT COUNT(*)"
        , "FROM aorb_answers"
        , "WHERE user_id = ?"
        , "AND date(substr(answered_on, 1, 10)) = date('now')"
        ]
  counts <- SQL.query conn query [uid] :: IO [SQL.Only Int]
  case counts of
    (count:_) -> return $ SQL.fromOnly count
    [] -> return 0

getUserAorbAndAssoc :: SQL.Connection -> UserID
                    -> IO (Maybe (Aorb, AssociationScheme))
getUserAorbAndAssoc conn uid = do
  let query = "SELECT aorb_id, assoc FROM users WHERE id = ?"
  aorbIdAssoc <- SQL.query conn query [uid] :: IO [(AorbID, AssociationScheme)]
  case aorbIdAssoc of
    [(aid, assoc)] -> do
      let aorbQuery = SQL.Query $ T.unwords
                    [ "SELECT a.id, a.context, a.subtext, a.a, a.b, a.mean"
                    , "FROM aorb"
                    , "WHERE id = ?"
                    ]
      aorb <- SQL.query conn aorbQuery [aid] :: IO [Aorb]
      case aorb of
        [aorb'] -> return $ Just (aorb', assoc)
        _ -> return Nothing
    [] -> return Nothing
    _ -> return Nothing

getUserTopXMostCommonplace :: SQL.Connection -> UserID -> Int
                           -> IO [AorbWithAnswer]
getUserTopXMostCommonplace conn uid x = do
  let query = SQL.Query $ T.unwords
        [ "SELECT a.id, a.context, a.subtext, a.a, a.b, a.mean, ans.answer"
        , "FROM aorb a"
        , "JOIN aorb_answers ans ON a.id = ans.aorb_id"
        , "WHERE ans.user_id = ?"
        , "ORDER BY ABS(CAST(ans.answer AS REAL) - a.mean) ASC"
        , "LIMIT ?"
        ]
  SQL.query conn query (uid, x)

getUserTopXMostControversial :: SQL.Connection -> UserID -> Int
                             -> IO [AorbWithAnswer]
getUserTopXMostControversial conn uid x = do
  let query = SQL.Query $ T.unwords
        [ "SELECT a.id, a.context, a.subtext, a.a, a.b, a.mean, ans.answer"
        , "FROM aorb a"
        , "JOIN aorb_answers ans ON a.id = ans.aorb_id"
        , "WHERE ans.user_id = ?"
        , "ORDER BY ABS(CAST(ans.answer AS REAL) - a.mean) DESC"
        , "LIMIT ?"
        ]
  SQL.query conn query (uid, x)

getUserAorbsFromControversialToCommonPlace :: SQL.Connection -> UserID
                                           -> IO [AorbWithAnswer]
getUserAorbsFromControversialToCommonPlace conn uid = do
  let query = SQL.Query $ T.unwords
        [ "SELECT a.id, a.context, a.subtext, a.a, a.b, a.mean, ans.answer"
        , "FROM aorb a"
        , "JOIN aorb_answers ans ON a.id = ans.aorb_id"
        , "WHERE ans.user_id = ?"
        , "ORDER BY ABS(CAST(ans.answer AS REAL) - a.mean) DESC"
        ]
  SQL.query conn query [uid]

getMatchesMainAorbs ::
  SQL.Connection -> UserID -> UserID
  -> IO (Maybe (MatchingAorbWithAnswer, MatchingAorbWithAnswer))
getMatchesMainAorbs conn uid1 uid2 = do
  let userQuery = SQL.Query "SELECT aorb_id FROM users WHERE id IN (?, ?)"
  aorbIds <- SQL.query conn userQuery (uid1, uid2) :: IO [SQL.Only Int]
  case aorbIds of
    [SQL.Only aid1, SQL.Only aid2] -> do
      let aorbQuery = SQL.Query $ T.unwords
            [ "SELECT a.id, a.context, a.subtext, a.a, a.b, a.mean,"
            , "ans1.answer as user1_answer,"
            , "ans2.answer as user2_answer"
            , "FROM aorb a"
            , "JOIN aorb_answers ans1"
            , "ON a.id = ans1.aorb_id AND ans1.user_id = ?"
            , "JOIN aorb_answers ans2"
            , "ON a.id = ans2.aorb_id AND ans2.user_id = ?"
            , "WHERE a.id = ?"
            ]
      aorb1s <- SQL.query
        conn aorbQuery (uid1, uid2, aid1) :: IO [MatchingAorbWithAnswer]
      aorb2s <- SQL.query
        conn aorbQuery (uid1, uid2, aid2) :: IO [MatchingAorbWithAnswer]
      case (aorb1s, aorb2s) of
        ([a1], [a2]) -> return $ Just (a1, a2)
        _ -> return Nothing
    _ -> return Nothing

getMatchesTopXCommonDisagreement :: SQL.Connection -> UserID -> UserID -> Int
                                 -> IO [MatchingAorbWithAnswer]
getMatchesTopXCommonDisagreement conn uid1 uid2 x = do
  let query = SQL.Query $ T.unwords
        [ "WITH disagreements AS ("
        , "  SELECT b.id, b.context, b.subtext, b.a, b.b, b.mean,"
        , "         a1.answer as main_answer, a2.answer as other_answer,"
        , "         mean * (1 - mean) as variance"
        , "  FROM aorb_answers a1"
        , "  JOIN aorb_answers a2 ON a1.aorb_id = a2.aorb_id"
        , "  JOIN aorb b ON a1.aorb_id = b.id"
        , "  WHERE a1.user_id = ?"
        , "  AND a2.user_id = ?"
        , "  AND a1.answer != a2.answer"
        , ")"
        , "SELECT id, context, subtext, a, b, mean, main_answer, other_answer"
        , "FROM disagreements"
        , "ORDER BY variance DESC"
        , "LIMIT ?"
        ]
  SQL.query conn query (uid1, uid2, x)

getMatchesTopXUniqueAgreement :: SQL.Connection -> UserID -> UserID -> Int
                              -> IO [MatchingAorbWithAnswer]
getMatchesTopXUniqueAgreement conn uid1 uid2 x = do
  let query = SQL.Query $ T.unwords
        [ "WITH matching_answers AS ("
        , "  SELECT b.id, b.context, b.subtext, b.a, b.b, b.mean,"
        , "         a1.answer as main_answer, a2.answer as other_answer,"
        , "         ABS(a1.answer - b.mean) as difference"
        , "  FROM aorb_answers a1"
        , "  JOIN aorb_answers a2 ON a1.aorb_id = a2.aorb_id"
        , "  JOIN aorb b ON a1.aorb_id = b.id"
        , "  WHERE a1.user_id = ?"
        , "  AND a2.user_id = ?"
        , "  AND a1.answer = a2.answer"
        , ")"
        , "SELECT id, context, subtext, a, b, mean, main_answer, other_answer"
        , "FROM matching_answers"
        , "ORDER BY difference DESC"
        , "LIMIT ?"
        ]
  SQL.query conn query (uid1, uid2, x)

getLargestIntersection :: SQL.Connection -> UserID -> UserID -> IO [AorbID]
getLargestIntersection conn uid1 uid2 = do
  let query = SQL.Query $ T.unwords
        [ "SELECT aorb_id"
        , "FROM aorb_answers"
        , "WHERE user_id = ?"
        , "INTERSECT"
        , "SELECT aorb_id"
        , "FROM aorb_answers"
        , "WHERE user_id = ?"
        ]
  rows <- SQL.query conn query (uid1, uid2) :: IO [SQL.Only AorbID]
  return $ map SQL.fromOnly rows

getLargestIntersectionAnswers :: SQL.Connection -> UserID -> UserID
                              -> IO ([AorbAnswer], [AorbAnswer])
getLargestIntersectionAnswers conn uid1 uid2 = do
  intersection <- getLargestIntersection conn uid1 uid2
  if null intersection
    then return ([], [])
    else do
      let query =
            SQL.Query $ T.unwords
              [ "SELECT user_id, aorb_id, answer"
              , "FROM aorb_answers"
              , "WHERE user_id IN (?, ?)"
              , "AND aorb_id IN ("
              , T.intercalate "," (map (T.pack . show) intersection)
              , ")"
              , "ORDER BY aorb_id, user_id"
              ]
      rows <- SQL.query
        conn query (uid1, uid2) :: IO [(UserID, AorbID, AorbAnswer)]
      let (answers1, answers2) =
            List.partition (\(uid, _, _) -> uid == uid1) rows
      return (map (\(_, _, ans) -> ans) answers1,
              map (\(_, _, ans) -> ans) answers2)
