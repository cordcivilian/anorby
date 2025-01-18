{-# LANGUAGE OverloadedStrings #-}

module Anorby where

import qualified System.Random as Random

import qualified Data.Word as Word
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Aeson as JSON
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Pool as Pool
import qualified Data.Time as Time
import qualified Data.Time.Clock.POSIX as POSIXTime

import qualified Control.Monad as Monad

import qualified Database.SQLite.Simple as SQL
import qualified Database.SQLite.Simple.FromField as SQL
import qualified Database.SQLite.Simple.ToField as SQL
import qualified Database.SQLite.Simple.Ok as SQL

-- | AorbAnswer: 0 corresponds to choice A, 1 corresponds to choice B
newtype AorbAnswer = AorbAnswer Word.Word8 deriving (Show, Eq, Ord)
type BinaryVector = [AorbAnswer]
type Contingency = (Int, Int, Int, Int)
type WeightVector = [Double]
type WeightedContingency = (Double, Double, Double, Double)
type SimilarityScore = Double
type BinaryVectorSimilarity =
  BinaryVector -> BinaryVector -> WeightVector -> SimilarityScore
data AssociationScheme = PPPod | Balance | Bipolar
  deriving (Eq, Show, Enum, Bounded)
type AorbID = Int
type UserID = Int
type UserSubmission = (BinaryVector, AorbID, AssociationScheme)
type Submissions = Map.Map UserID UserSubmission
type Ranking = [UserID]
type Rankings = Map.Map UserID Ranking
type Marriages = Map.Map UserID (Maybe UserID)

data AnswerToken = AnswerToken
  { tokenUserId :: UserID
  , tokenAorbId :: AorbID
  , tokenExpiry :: POSIXTime.POSIXTime
  } deriving (Show)

data SubmitAnswer = SubmitAnswer
  { submitAorbId :: AorbID
  , submitChoice :: Word.Word8
  , submitToken :: T.Text
  } deriving (Show)

instance SQL.FromField AssociationScheme where
  fromField f = do
    assocInt <- SQL.fromField f
    case assocInt :: Int of
      1  -> SQL.Ok PPPod
      0  -> SQL.Ok Balance
      -1 -> SQL.Ok Bipolar
      _  -> SQL.Ok Balance

instance SQL.ToField AssociationScheme where
  toField PPPod   = SQL.SQLInteger 1
  toField Balance = SQL.SQLInteger 0
  toField Bipolar = SQL.SQLInteger (-1)

data User = User
  { userId :: UserID
  , userName :: T.Text
  , userEmail :: T.Text
  , userUuid :: T.Text
  , userAorbId :: AorbID
  , userAssoc :: AssociationScheme
  } deriving (Show)

instance SQL.FromRow User where
  fromRow =
    User
    <$> SQL.field
    <*> SQL.field
    <*> SQL.field
    <*> SQL.field
    <*> SQL.field
    <*> SQL.field

instance SQL.ToRow User where
  toRow user =
    [ SQL.SQLInteger (fromIntegral $ userId user)
    , SQL.SQLText (userName user)
    , SQL.SQLText (userEmail user)
    , SQL.SQLText (userUuid user)
    , SQL.SQLInteger (fromIntegral $ userAorbId user)
    , SQL.toField (userAssoc user)
    ]

data Aorb = Aorb
  { aorbId :: AorbID
  , aorbCtx :: T.Text
  , aorbStx :: T.Text
  , aorbA :: T.Text
  , aorbB :: T.Text
  , aorbMean :: Double
  } deriving (Show)

instance Eq Aorb where a1 == a2 = aorbId a1 == aorbId a2

instance SQL.FromRow Aorb where
  fromRow =
    Aorb
    <$> SQL.field
    <*> SQL.field
    <*> SQL.field
    <*> SQL.field
    <*> SQL.field
    <*> SQL.field

instance SQL.ToRow Aorb where
  toRow aorb =
    [ SQL.SQLInteger (fromIntegral $ aorbId aorb)
    , SQL.SQLText (aorbCtx aorb)
    , SQL.SQLText (aorbStx aorb)
    , SQL.SQLText (aorbA aorb)
    , SQL.SQLText (aorbB aorb)
    ]

data AorbAnswers = AorbAnswers
  { aorbUserId :: UserID
  , aorbAorbId :: AorbID
  , aorbAnswer :: AorbAnswer
  , aorbAnsweredOn :: Time.UTCTime
  } deriving (Show)

instance SQL.FromRow AorbAnswers where
  fromRow =
    AorbAnswers
    <$> SQL.field
    <*> SQL.field
    <*> SQL.field
    <*> SQL.field

instance SQL.ToRow AorbAnswers where
  toRow ans =
    [ SQL.SQLInteger (fromIntegral $ aorbUserId ans)
    , SQL.SQLInteger (fromIntegral $ aorbAorbId ans)
    , SQL.toField (aorbAnswer ans)
    , SQL.SQLText (T.pack $ show $ aorbAnsweredOn ans)
    ]

instance SQL.FromField AorbAnswer where
  fromField f = do
    val <- SQL.fromField f
    return $ AorbAnswer (if val == (1 :: Int) then 1 else 0 :: Word.Word8)

instance SQL.ToField AorbAnswer where
  toField (AorbAnswer w) =
    SQL.SQLInteger $
      fromIntegral (if w == (1 :: Word.Word8) then 1 else 0 :: Word.Word8)

data AorbWithAnswer = AorbWithAnswer
  { aorbData :: Aorb
  , userAnswer :: AorbAnswer
  } deriving (Show)

instance Eq AorbWithAnswer where
  a1 == a2 = aorbData a1 == aorbData a2 && userAnswer a1 == userAnswer a2

instance SQL.FromRow AorbWithAnswer where
  fromRow = AorbWithAnswer
    <$> (Aorb
          <$> SQL.field
          <*> SQL.field
          <*> SQL.field
          <*> SQL.field
          <*> SQL.field
          <*> SQL.field)
    <*> SQL.field

data MatchingAorbWithAnswer = MatchingAorbWithAnswer
  { matchingAorbData :: Aorb
  , mainUserAnswer :: AorbAnswer
  , otherUserAnswer :: AorbAnswer
  } deriving (Show)

instance SQL.FromRow MatchingAorbWithAnswer where
  fromRow = MatchingAorbWithAnswer
    <$> (Aorb
          <$> SQL.field
          <*> SQL.field
          <*> SQL.field
          <*> SQL.field
          <*> SQL.field
          <*> SQL.field)
    <*> SQL.field
    <*> SQL.field

-- ---------------------------------------------------------------------------

instance JSON.FromJSON Aorb where
  parseJSON = JSON.withObject "Aorb" $ \v -> Aorb
    <$> v JSON..: "id"
    <*> v JSON..: "context"
    <*> v JSON..: "subtext"
    <*> v JSON..: "a"
    <*> v JSON..: "b"
    <*> pure 0.500000000000000

instance JSON.FromJSON SubmitAnswer where
  parseJSON = JSON.withObject "SubmitAnswer" $ \v -> SubmitAnswer
    <$> v JSON..: "aorb_id"
    <*> v JSON..: "choice"
    <*> v JSON..: "token"

-- ---------------------------------------------------------------------------

instance Random.Random AorbAnswer where
  random g =
    case Random.random g of
      (b, g') -> (if b then AorbAnswer 1 else AorbAnswer 0, g')
  randomR (_, _) g =
    case Random.random g of
      (b, g') -> (if b then AorbAnswer 1 else AorbAnswer 0, g')

instance Random.Random AssociationScheme where
  random g = Random.randomR (minBound, maxBound) g
  randomR (a, b) g =
    case Random.randomR (fromEnum a, fromEnum b) g of
      (x, g') -> (toEnum x, g')

-- ---------------------------------------------------------------------------

initDB :: FilePath -> IO SQL.Connection
initDB db = do
  conn <- SQL.open db
  initSQLitePragmas conn
  return conn

initConn :: FilePath -> IO SQL.Connection
initConn db = SQL.open db

initPool :: FilePath -> IO (Pool.Pool SQL.Connection)
initPool db = Pool.newPool $
  Pool.defaultPoolConfig (initConn db) SQL.close 30 10

-- | à la https://briandouglas.ie/sqlite-defaults/
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

initTables :: SQL.Connection -> IO ()
initTables conn = SQL.withTransaction conn $ do
  initUserTable conn
  initAorbTable conn
  initAorbAnswersTable conn
  initAorbMeanTrigger conn

initUserTable :: SQL.Connection -> IO()
initUserTable conn = SQL.execute_ conn $ SQL.Query $ T.unwords
 [ "CREATE TABLE IF NOT EXISTS users"
 , "(id INTEGER PRIMARY KEY,"
 , "name TEXT NOT NULL,"
 , "email TEXT NOT NULL,"
 , "uuid TEXT NOT NULL,"
 , "aorb_id INTEGER NOT NULL,"
 , "assoc INTEGER NOT NULL,"
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
initAorbMeanTrigger conn = SQL.execute_ conn $ SQL.Query $ T.unwords
 [ "CREATE TRIGGER IF NOT EXISTS update_aorb_mean"
 , "AFTER INSERT ON aorb_answers BEGIN"
 , "UPDATE aorb"
 , "SET mean = (SELECT AVG(answer)"
 , "FROM aorb_answers"
 , "WHERE aorb_id = NEW.aorb_id)"
 , "WHERE id = NEW.aorb_id;"
 , "END"
 ]

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

-- ---------------------------------------------------------------------------

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

getUsersAnswers :: SQL.Connection -> [UserID]
                -> IO (Map.Map UserID [AorbAnswers])
getUsersAnswers conn users = do
  let placeholders = replicate (length users) "?"
      inClause = T.intercalate "," placeholders
      query = SQL.Query $ T.unwords
        [ "SELECT user_id, aorb_id, answer"
        , "FROM aorb_answers"
        , "WHERE user_id IN (" <> inClause <> ")"
        ]
  answers <- SQL.query conn query users :: IO [AorbAnswers]
  return $
    Map.fromListWith (++) $ map (\ans -> (aorbUserId ans, [ans])) answers

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

baseAorbsToSubmissions :: SQL.Connection -> IO Submissions
baseAorbsToSubmissions conn = do
  basedUsers <- getUsersWithAnswers conn [1..100]
  basedUsersAnswers <- getUsersAnswers conn basedUsers
  basedUsersAorbIdAssoc <- getUsersAorbIdAndAssoc conn basedUsers
  return $ Map.mapWithKey (\uid (aid, assoc) ->
    let answers =
          maybe [] (map aorbAnswer) $ Map.lookup uid basedUsersAnswers
    in (answers, aid, assoc)
    ) basedUsersAorbIdAssoc

-- ---------------------------------------------------------------------------

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

getNextUnansweredAorb :: SQL.Connection -> UserID -> IO (Maybe Aorb)
getNextUnansweredAorb conn uid = do
  let query = SQL.Query $ T.unwords
        [ "SELECT a.*"
        , "FROM aorb a"
        , "WHERE NOT EXISTS ("
        , "  SELECT 1"
        , "  FROM aorb_answers ans"
        , "  WHERE ans.aorb_id = a.id"
        , "  AND ans.user_id = ?"
        , ")"
        , "ORDER BY RANDOM()"
        , "LIMIT 1"
        ]
  results <- SQL.query conn query [uid] :: IO [Aorb]
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

getMatchesMainAorbs :: SQL.Connection -> UserID -> UserID
                    -> IO (Maybe
                            (MatchingAorbWithAnswer, MatchingAorbWithAnswer)
                          )
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
