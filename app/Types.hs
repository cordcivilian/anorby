{-# LANGUAGE OverloadedStrings #-}

module Types where

import qualified Data.Aeson as JSON
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Time.Clock.POSIX as POSIXTime
import qualified Data.Word as Word
import qualified System.Random as Random

import qualified Database.SQLite.Simple as SQL
import qualified Database.SQLite.Simple.FromField as SQL
import qualified Database.SQLite.Simple.ToField as SQL
import qualified Database.SQLite.Simple.Ok as SQL

-- | Core Types

newtype AorbAnswer = AorbAnswer Word.Word8
  deriving (Show, Eq, Ord)

type BinaryVector = [AorbAnswer]
type ValidPairCount = Int
type WeightedValidPairCount = Int
type Contingency = (Int, Int, Int, Int)
type WeightVector = [Int]
type WeightedContingency = (Int, Int, Int, Int)
type SimilarityScore = Double
type BinaryVectorSimilarity =
  BinaryVector -> BinaryVector -> WeightVector -> SimilarityScore

type WeightedContingencyWithValid =
  (WeightedContingency, WeightedValidPairCount)

data AssociationScheme = PPPod | Fencer | Bipolar
  deriving (Eq, Show, Enum, Bounded)

type AorbID = Int
type UserID = Int
type UserSubmission = (BinaryVector, AorbID, AssociationScheme)
type Submissions = Map.Map UserID UserSubmission
type Ranking = [UserID]
type Rankings = Map.Map UserID Ranking
type Marriages = Map.Map UserID (Maybe UserID)

type OrderingFunction a = [a] -> [a]

-- | Database Types

data User = User
  { userId :: UserID
  , userName :: T.Text
  , userEmail :: T.Text
  , userUuid :: T.Text
  , userAorbId :: Maybe AorbID
  , userAssoc :: Maybe AssociationScheme
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
  , aorbAnsweredOn :: POSIXTime.POSIXTime
  } deriving (Show)

data AorbWithAnswer = AorbWithAnswer
  { aorbData :: Aorb
  , userAnswer :: AorbAnswer
  } deriving (Show)

data MatchingAorbWithAnswer = MatchingAorbWithAnswer
  { matchingAorbData :: Aorb
  , mainUserAnswer :: AorbAnswer
  , otherUserAnswer :: AorbAnswer
  } deriving (Show)

data Match = Match
  { matchUserId :: UserID
  , matchTargetId :: UserID
  , matchTimestamp :: POSIXTime.POSIXTime
  } deriving (Show, Eq)

-- | Auth Types

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

-- | Type Class Instances (Random)

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

-- | Type Class Instances

instance Eq Aorb where
  a1 == a2 = aorbId a1 == aorbId a2

instance Eq AorbWithAnswer where
  a1 == a2 = aorbData a1 == aorbData a2 && userAnswer a1 == userAnswer a2

instance SQL.FromField AssociationScheme where
  fromField f = do
    assocInt <- SQL.fromField f
    case assocInt :: Int of
      1  -> SQL.Ok PPPod
      0  -> SQL.Ok Fencer
      -1 -> SQL.Ok Bipolar
      _  -> SQL.Ok Fencer

instance SQL.ToField AssociationScheme where
  toField PPPod   = SQL.SQLInteger 1
  toField Fencer = SQL.SQLInteger 0
  toField Bipolar = SQL.SQLInteger (-1)

instance SQL.FromRow User where
  fromRow = User
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
    , maybe SQL.SQLNull (SQL.SQLInteger . fromIntegral) (userAorbId user)
    , maybe SQL.SQLNull SQL.toField (userAssoc user)
    ]

instance SQL.FromRow Aorb where
  fromRow = Aorb
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

instance SQL.FromRow AorbAnswers where
  fromRow = AorbAnswers
    <$> SQL.field
    <*> SQL.field
    <*> SQL.field
    <*> (read . T.unpack <$> SQL.field)

instance SQL.ToRow AorbAnswers where
  toRow ans =
    [ SQL.SQLInteger (fromIntegral $ aorbUserId ans)
    , SQL.SQLInteger (fromIntegral $ aorbAorbId ans)
    , SQL.toField (aorbAnswer ans)
    , SQL.SQLText $ T.pack $ show $ aorbAnsweredOn ans
    ]

instance SQL.FromField AorbAnswer where
  fromField f = do
    val <- SQL.fromField f
    return $ AorbAnswer (if val == (1 :: Int) then 1 else 0 :: Word.Word8)

instance SQL.ToField AorbAnswer where
  toField (AorbAnswer w) =
    SQL.SQLInteger $
      fromIntegral (if w == (1 :: Word.Word8) then 1 else 0 :: Word.Word8)

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

instance SQL.FromRow Match where
  fromRow = Match
    <$> SQL.field
    <*> SQL.field
    <*> (read . T.unpack <$> SQL.field)

instance SQL.ToRow Match where
  toRow match =
    [ SQL.SQLInteger (fromIntegral $ matchUserId match)
    , SQL.SQLInteger (fromIntegral $ matchTargetId match)
    , SQL.SQLText $ T.pack $ show $ matchTimestamp match
    ]

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
