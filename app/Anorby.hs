{-# LANGUAGE OverloadedStrings #-}

module Anorby where

import qualified System.Random as Random

import qualified Data.Time as Time
import qualified Data.Time.Format as DateTimeFormat
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Aeson as JSON

import qualified Control.Monad as Monad

import qualified Database.SQLite.Simple as SQL
import qualified Database.SQLite.Simple.FromField as SQL
import qualified Database.SQLite.Simple.ToField as SQL
import qualified Database.SQLite.Simple.Ok as SQL

data User = User
  { userId :: Int
  , userName :: T.Text
  , userEmail :: T.Text
  , userAorbId :: Int
  , userAssoc :: AssociationScheme
  } deriving (Show)

data Aorb = Aorb
  { aorbId :: Int
  , aorbCtx :: T.Text
  , aorbStx :: T.Text
  , aorbA :: T.Text
  , aorbB :: T.Text
  , aorbMean :: Double
  } deriving (Show)

data AorbAnswers = AorbAnswers
  { aorbUserId :: Int
  , aorbAorbId :: Int
  , aorbAnswer :: Bool
  } deriving (Show)

data AssociationScheme = PPPod | Balance | Bipolar 
  deriving (Eq, Show, Enum, Bounded)

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
    , SQL.SQLInteger (if aorbAnswer ans then 1 else 0)
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

-- ---------------------------------------------------------------------------

mockBaseAorbAnswers :: Int -> IO ()
mockBaseAorbAnswers n = do
  now <- Time.getCurrentTime
  let timestamp = DateTimeFormat.formatTime 
                  DateTimeFormat.defaultTimeLocale 
                  "%Y%m%d%H%M%S" 
                  now
      dbName = "data/test-anorby-" ++ timestamp ++ ".db"
  conn <- initDB dbName
  initTables conn
  mockUsers conn n
  ingestBaseAorbData conn
  users <- SQL.query_ conn "SELECT * FROM users" :: IO [User]
  aorbs <- SQL.query_ conn "SELECT * FROM aorb" :: IO [Aorb]
  mockAorbAnswers conn aorbs users

instance Random.Random AssociationScheme where
  random g = Random.randomR (minBound, maxBound) g
  randomR (a, b) g =
    case Random.randomR (fromEnum a, fromEnum b) g of
      (x, g') -> (toEnum x, g')

mockUsers :: SQL.Connection -> Int -> IO ()
mockUsers conn n = do
  gen <- Random.getStdGen
  (users, _) <- generateMockUsers n gen
  SQL.withTransaction conn $ SQL.executeMany conn
    "INSERT OR REPLACE INTO users (id, name, email, aorb_id, assoc) VALUES (?, ?, ?, ?, ?)"
    users
  where
    generateMockUsers :: Int -> Random.StdGen -> IO ([User], Random.StdGen)
    generateMockUsers count g = do
      Monad.foldM (\(accUsers, currGen) i -> do
        let (mockAssoc, g2) = Random.random currGen
            (mockAorbId, g3) = Random.randomR (1, 100) g2
            user = User
              { userId = i + 1
              , userName = T.pack $ "User" ++ show (i + 1)
              , userEmail = T.pack $ "user" ++ show (i + 1) ++ "@example.com"
              , userAorbId = mockAorbId
              , userAssoc = mockAssoc
              }
        return (user : accUsers, g3)
        ) ([], g) [0..count-1]

mockAorbAnswers :: SQL.Connection -> [Aorb] -> [User] -> IO ()
mockAorbAnswers conn aorbs users = do
  gen <- Random.getStdGen
  (answers, _) <- generateMockAnswers gen
  SQL.withTransaction conn $ SQL.executeMany conn
    "INSERT OR REPLACE INTO aorb_answers (user_id, aorb_id, answer) VALUES (?, ?, ?)"
    answers
  where
    generateMockAnswers :: Random.StdGen -> IO ([AorbAnswers], Random.StdGen)
    generateMockAnswers g = do
      Monad.foldM (\(accAnswers, currGen) (u, a) -> do
        let (answer, nextGen) = Random.random currGen
            mockAorbAnswer= AorbAnswers
              { aorbUserId = userId u
              , aorbAorbId = aorbId a  
              , aorbAnswer = answer
              }
        return (mockAorbAnswer : accAnswers, nextGen)
        ) ([], g) [(u, a) | u <- users, a <- aorbs]
