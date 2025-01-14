{-# LANGUAGE OverloadedStrings #-}

module Anorby where

import qualified System.Random as Random

import qualified Data.Word as Word
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Aeson as JSON
import qualified Data.Map as Map

import qualified Database.SQLite.Simple as SQL
import qualified Database.SQLite.Simple.FromField as SQL
import qualified Database.SQLite.Simple.ToField as SQL
import qualified Database.SQLite.Simple.Ok as SQL

type SimilarityScore = Double
type AorbAnswer = Word.Word8
type BinaryVector = [AorbAnswer]
type Contingency = (Int, Int, Int, Int)
type WeightVector = [Double]
type WeightedContingency = (Double, Double, Double, Double)
type BinaryVectorSimilarity =
  BinaryVector -> BinaryVector -> WeightVector -> SimilarityScore

type AorbID = Int
type UserID = Int
type UserSubmission = (BinaryVector, AorbID, AssociationScheme)
type Submissions = Map.Map UserID UserSubmission
type Ranking = [UserID]
type Rankings = Map.Map UserID Ranking
type Marriages = Map.Map UserID (Maybe UserID)

-- ---------------------------------------------------------------------------

data User = User
  { userId :: UserID
  , userName :: T.Text
  , userEmail :: T.Text
  , userAorbId :: AorbID
  , userAssoc :: AssociationScheme
  } deriving (Show)

data Aorb = Aorb
  { aorbId :: AorbID
  , aorbCtx :: T.Text
  , aorbStx :: T.Text
  , aorbA :: T.Text
  , aorbB :: T.Text
  , aorbMean :: Double
  } deriving (Show)

data AorbAnswers = AorbAnswers
  { aorbUserId :: UserID
  , aorbAorbId :: AorbID
  , aorbAnswer :: AorbAnswer
  } deriving (Show)

data AssociationScheme = PPPod | Balance | Bipolar 
  deriving (Eq, Show, Enum, Bounded)

instance Random.Random AssociationScheme where
  random g = Random.randomR (minBound, maxBound) g
  randomR (a, b) g =
    case Random.randomR (fromEnum a, fromEnum b) g of
      (x, g') -> (toEnum x, g')

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

-- ---------------------------------------------------------------------------

instance SQL.FromRow User where
  fromRow =
    User
    <$> SQL.field
    <*> SQL.field
    <*> SQL.field
    <*> SQL.field
    <*> SQL.field

instance SQL.FromRow Aorb where
  fromRow =
    Aorb
    <$> SQL.field
    <*> SQL.field
    <*> SQL.field
    <*> SQL.field
    <*> SQL.field
    <*> SQL.field

instance SQL.FromRow AorbAnswers where
  fromRow =
    AorbAnswers
    <$> SQL.field
    <*> SQL.field
    <*> SQL.field

-- ---------------------------------------------------------------------------

instance SQL.ToRow User where
  toRow user = 
    [ SQL.SQLInteger (fromIntegral $ userId user)
    , SQL.SQLText (userName user)
    , SQL.SQLText (userEmail user)
    , SQL.SQLInteger (fromIntegral $ userAorbId user)
    , SQL.toField (userAssoc user)
    ]

instance SQL.ToRow Aorb where
  toRow aorb =
    [ SQL.SQLInteger (fromIntegral $ aorbId aorb)
    , SQL.SQLText (aorbCtx aorb)
    , SQL.SQLText (aorbStx aorb)
    , SQL.SQLText (aorbA aorb)
    , SQL.SQLText (aorbB aorb)
    ]

instance SQL.ToRow AorbAnswers where
  toRow ans =
    [ SQL.SQLInteger (fromIntegral $ aorbUserId ans)
    , SQL.SQLInteger (fromIntegral $ aorbAorbId ans)
    , SQL.SQLInteger (fromIntegral $ aorbAnswer ans)
    ]

-- ---------------------------------------------------------------------------

instance JSON.FromJSON Aorb where
  parseJSON = JSON.withObject "Aorb" $ \v -> Aorb
    <$> v JSON..: "id"
    <*> v JSON..: "context"
    <*> v JSON..: "subtext"
    <*> v JSON..: "a"
    <*> v JSON..: "b"
    <*> pure 0.500000000000000

-- ---------------------------------------------------------------------------

initDB :: FilePath -> IO SQL.Connection
initDB db = SQL.open db

initTables :: SQL.Connection -> IO ()
initTables conn = SQL.withTransaction conn $ do
  initUserTable conn
  initAorbTable conn
  initAorbAnswersTable conn
  initAorbMeanTrigger conn

initUserTable :: SQL.Connection -> IO()
initUserTable conn = SQL.execute_ conn query
  where query = "CREATE TABLE IF NOT EXISTS users (id INTEGER PRIMARY KEY, name TEXT NOT NULL, email TEXT NOT NULL, aorb_id INTEGER NOT NULL, assoc INTEGER NOT NULL, UNIQUE(email))"

initAorbTable :: SQL.Connection -> IO()
initAorbTable conn = SQL.execute_ conn query
  where query = "CREATE TABLE IF NOT EXISTS aorb (id INTEGER PRIMARY KEY, context TEXT NOT NULL, subtext TEXT NOT NULL, a TEXT NOT NULL, b TEXT NOT NULL, mean REAL NOT NULL DEFAULT 0.500000000000000)"

initAorbAnswersTable :: SQL.Connection -> IO()
initAorbAnswersTable conn = SQL.execute_ conn query
  where query = "CREATE TABLE IF NOT EXISTS aorb_answers (user_id INTEGER NOT NULL, aorb_id INTEGER NOT NULL, answer INTEGER NOT NULL, FOREIGN KEY (user_id) REFERENCES users(id), FOREIGN KEY (aorb_id) REFERENCES aorb(id), UNIQUE(user_id, aorb_id))"

initAorbMeanTrigger :: SQL.Connection -> IO ()
initAorbMeanTrigger conn = SQL.execute_ conn query
  where query = "CREATE TRIGGER IF NOT EXISTS update_aorb_mean AFTER INSERT ON aorb_answers BEGIN UPDATE aorb SET mean = (SELECT AVG(answer) FROM aorb_answers WHERE aorb_id = NEW.aorb_id) WHERE id = NEW.aorb_id; END"

readBaseAorbs :: IO (Maybe [Aorb])
readBaseAorbs = do
  jsonData <- BSL.readFile "data/base.json"
  return $ JSON.decode jsonData

ingestBaseAorbData :: SQL.Connection -> IO ()
ingestBaseAorbData conn = do
  base <- readBaseAorbs 
  case base of
    Just aorbs -> SQL.withTransaction conn $ SQL.executeMany conn
      "INSERT OR REPLACE INTO aorb (id, context, subtext, a, b) VALUES (?, ?, ?, ?, ?)"
      (map (\aorb ->
        (aorbId aorb, aorbCtx aorb, aorbStx aorb, aorbA aorb, aorbB aorb)
           ) aorbs)
    Nothing -> putStrLn "error parsing json"

getUsersWithAnswers :: SQL.Connection -> [AorbID] -> IO [UserID]
getUsersWithAnswers conn aorbIds = do
  let placeholders = replicate (length aorbIds) "?"
      inClause = T.intercalate "," placeholders
      query = SQL.Query $
        "SELECT user_id FROM aorb_answers WHERE aorb_id IN ("
        <> inClause <>
        ") GROUP BY user_id HAVING COUNT(DISTINCT aorb_id) = ?"
      params = aorbIds ++ [length aorbIds]
  users <- SQL.query conn query params :: IO [SQL.Only UserID]
  return $ map SQL.fromOnly users

getUsersAnswers :: SQL.Connection -> [UserID]
                -> IO (Map.Map UserID [AorbAnswers])
getUsersAnswers conn users = do
  let placeholders = replicate (length users) "?"
      inClause = T.intercalate "," placeholders
      query = SQL.Query $
        "SELECT user_id, aorb_id, answer FROM aorb_answers WHERE user_id IN ("
        <> inClause <>
        ")"
  answers <- SQL.query conn query users :: IO [AorbAnswers]
  return $
    Map.fromListWith (++) $ map (\ans -> (aorbUserId ans, [ans])) answers

getUsersAorbAndAssoc :: SQL.Connection -> [UserID]
                     -> IO (Map.Map UserID (AorbID, AssociationScheme))
getUsersAorbAndAssoc conn users = do
  let placeholders = replicate (length users) "?"
      inClause = T.intercalate "," placeholders
      query = SQL.Query $
        "SELECT id, aorb_id, assoc FROM users WHERE id IN (" <> inClause <> ")"
  aorbs <- SQL.query
    conn query users :: IO [(UserID, AorbID, AssociationScheme)]
  return $ Map.fromList [(uid, (aid, assoc)) | (uid, aid, assoc) <- aorbs]

baseAorbsToSubmissions :: SQL.Connection -> IO Submissions
baseAorbsToSubmissions conn = do
  basedUsers <- getUsersWithAnswers conn [1..100]
  basedUsersAnswers <- getUsersAnswers conn basedUsers
  basedUsersAorbAssoc <- getUsersAorbAndAssoc conn basedUsers
  return $ Map.mapWithKey (\uid (aid, assoc) -> 
    let answers =
          maybe [] (map aorbAnswer) $ Map.lookup uid basedUsersAnswers
    in (answers, aid, assoc)
    ) basedUsersAorbAssoc