{-# LANGUAGE OverloadedStrings #-}

module Types where

import qualified Data.Aeson as JSON
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Word as Word
import qualified System.Random as Random

import qualified Database.SQLite.Simple as SQL
import qualified Database.SQLite.Simple.FromField as SQL
import qualified Database.SQLite.Simple.ToField as SQL
import qualified Database.SQLite.Simple.Ok as SQL

type BinaryVector = [AorbAnswer]
type ValidPairCount = Int
type WeightedValidPairCount = Int
type Contingency = (Int, Int, Int, Int)
type WeightVector = [Int]
type WeightedContingency = (Int, Int, Int, Int)
type SimilarityScore = Double
type BinaryVectorSimilarity = BinaryVector -> BinaryVector -> WeightVector -> SimilarityScore
type WeightedContingencyWithValid = (WeightedContingency, WeightedValidPairCount)
type AorbID = Int
type UserID = Int
type UserSubmission = (BinaryVector, AorbID, AssociationScheme)
type Submissions = Map.Map UserID UserSubmission
type Ranking = [UserID]
type Rankings = Map.Map UserID Ranking
type Marriages = Map.Map UserID (Maybe UserID)
type OrderingFunction a = [a] -> [a]

data AssociationScheme = PPPod | Swing | Bipolar deriving (Eq, Show, Enum, Bounded)

instance SQL.FromField AssociationScheme where
  fromField f = do
    assocInt <- SQL.fromField f
    case assocInt :: Int of
      1  -> SQL.Ok PPPod
      0  -> SQL.Ok Swing
      -1 -> SQL.Ok Bipolar
      _  -> SQL.Ok Swing

instance SQL.ToField AssociationScheme where
  toField PPPod = SQL.SQLInteger 1
  toField Swing = SQL.SQLInteger 0
  toField Bipolar = SQL.SQLInteger (-1)

instance Random.Random AssociationScheme where
  random g = Random.randomR (minBound, maxBound) g
  randomR (a, b) g =
    case Random.randomR (fromEnum a, fromEnum b) g of
      (x, g') -> (toEnum x, g')

data User = User
  { userId :: UserID
  , userName :: T.Text
  , userEmail :: T.Text
  , userUuid :: T.Text
  , userAorbId :: Maybe AorbID
  , userAssoc :: Maybe AssociationScheme
  , userCreatedOn :: Integer
  } deriving (Show)

instance SQL.FromRow User where
  fromRow = User
    <$> SQL.field
    <*> SQL.field
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

data Aorb = Aorb
  { aorbId :: AorbID
  , aorbCtx :: T.Text
  , aorbStx :: T.Text
  , aorbA :: T.Text
  , aorbB :: T.Text
  , aorbMean :: Double
  , aorbCreatedOn :: Integer
  } deriving (Show)

instance Eq Aorb where
  a1 == a2 = aorbId a1 == aorbId a2

instance SQL.FromRow Aorb where
  fromRow = Aorb
    <$> SQL.field
    <*> SQL.field
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

instance JSON.FromJSON Aorb where
  parseJSON = JSON.withObject "Aorb" $ \v -> Aorb
    <$> v JSON..: "id"
    <*> v JSON..: "context"
    <*> v JSON..: "subtext"
    <*> v JSON..: "a"
    <*> v JSON..: "b"
    <*> pure 0.500000000000000
    <*> pure 0

newtype AorbAnswer = AorbAnswer Word.Word8 deriving (Show, Eq, Ord)

instance SQL.FromField AorbAnswer where
  fromField f = do
    val <- SQL.fromField f
    return $ AorbAnswer (if val == (1 :: Int) then 1 else 0 :: Word.Word8)

instance SQL.ToField AorbAnswer where
  toField (AorbAnswer w) =
    SQL.SQLInteger $
      fromIntegral (if w == (1 :: Word.Word8) then 1 else 0 :: Word.Word8)

instance Random.Random AorbAnswer where
  random g =
    case Random.random g of
      (b, g') -> (if b then AorbAnswer 1 else AorbAnswer 0, g')
  randomR (_, _) g =
    case Random.random g of
      (b, g') -> (if b then AorbAnswer 1 else AorbAnswer 0, g')

data MockAorbAnswer = MockAorbAnswer
  { mockUserId :: UserID
  , mockAorbId :: AorbID
  , mockAnswer :: AorbAnswer
  }

instance SQL.ToRow MockAorbAnswer where
  toRow ans =
    [ SQL.SQLInteger (fromIntegral $ mockUserId ans)
    , SQL.SQLInteger (fromIntegral $ mockAorbId ans)
    , SQL.toField (mockAnswer ans)
    ]

data AorbAnswers = AorbAnswers
  { aorbUserId :: UserID
  , aorbAorbId :: AorbID
  , aorbAnswer :: AorbAnswer
  , aorbAnsweredOn :: Integer
  } deriving (Show)

instance SQL.FromRow AorbAnswers where
  fromRow = AorbAnswers
    <$> SQL.field
    <*> SQL.field
    <*> SQL.field
    <*> SQL.field

instance SQL.ToRow AorbAnswers where
  toRow ans =
    [ SQL.SQLInteger (fromIntegral $ aorbUserId ans)
    , SQL.SQLInteger (fromIntegral $ aorbAorbId ans)
    , SQL.toField (aorbAnswer ans)
    , SQL.SQLInteger (fromIntegral $ aorbAnsweredOn ans)
    ]

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
          <*> SQL.field
          <*> SQL.field)
    <*> SQL.field
    <*> SQL.field

data Match = Match
  { matchUserId :: UserID
  , matchTargetId :: UserID
  , matchTimestamp :: Integer
  } deriving (Show, Eq)

instance SQL.FromRow Match where
  fromRow = Match
    <$> SQL.field
    <*> SQL.field
    <*> SQL.field

instance SQL.ToRow Match where
  toRow match =
    [ SQL.SQLInteger (fromIntegral $ matchUserId match)
    , SQL.SQLInteger (fromIntegral $ matchTargetId match)
    , SQL.SQLInteger (fromIntegral $ matchTimestamp match)
    ]

data MatchView = MatchView
  { viewTimestamp :: Integer
  , viewAgreementRate :: Double
  , viewTargetTotalAnswers :: Int
  , viewYourTotalAnswers :: Int
  , viewMainAorbs :: Maybe (MatchingAorbWithAnswer, MatchingAorbWithAnswer)
  , viewTopAgreement :: Maybe MatchingAorbWithAnswer
  , viewTopDisagreement :: Maybe MatchingAorbWithAnswer
  , viewGuessAorbs :: [Aorb]
  , viewGuessResults :: [GuessResult]
  , viewStereoQuestions :: [Stereo]
  , viewStereoGuesses :: [StereoGuess]
  }

newtype MatchRecord = MatchRecord (Int, Match)
  deriving (Show, Eq)

instance SQL.FromRow MatchRecord where
  fromRow = do
    id' <- SQL.field
    userId' <- SQL.field
    targetId' <- SQL.field
    timestamp' <- SQL.field
    return $ MatchRecord (id', Match userId' targetId' timestamp')

data Guess = Guess
  { guessId :: Int
  , guessMatchId :: Int
  , guessUserId :: UserID
  , guessAorbId :: AorbID
  , guessAnswer :: AorbAnswer
  , guessCreatedOn :: Integer
  } deriving (Show, Eq)

instance SQL.FromRow Guess where
  fromRow = Guess
    <$> SQL.field
    <*> SQL.field
    <*> SQL.field
    <*> SQL.field
    <*> SQL.field
    <*> SQL.field

data GuessResult = GuessResult
  { guessResultAorb :: Aorb
  , guessResultGuess :: AorbAnswer
  , guessResultActual :: AorbAnswer
  , guessResultCorrect :: Bool
  } deriving (Show, Eq)

data Stereo = Stereo
  { stereoId :: Int
  , stereoCtx :: T.Text
  , stereoStx :: T.Text
  , stereoA :: T.Text
  , stereoB :: T.Text
  , stereoCreatedOn :: Integer
  } deriving (Show)

instance SQL.FromRow Stereo where
  fromRow = Stereo
    <$> SQL.field
    <*> SQL.field
    <*> SQL.field
    <*> SQL.field
    <*> SQL.field
    <*> SQL.field

instance JSON.FromJSON Stereo where
  parseJSON = JSON.withObject "Stereo" $ \v -> Stereo
    <$> v JSON..: "id"
    <*> v JSON..: "context"
    <*> v JSON..: "subtext"
    <*> v JSON..: "a"
    <*> v JSON..: "b"
    <*> pure 0

data StereoGuess = StereoGuess
  { stereoGuessId :: Int
  , stereoGuessMatchId :: Int
  , stereoGuessUserId :: UserID
  , stereoGuessTargetId :: UserID
  , stereoGuessStereoId :: Int
  , stereoGuessAnswer :: AorbAnswer
  , stereoGuessCreatedOn :: Integer
  } deriving (Show, Eq)

instance SQL.FromRow StereoGuess where
  fromRow = StereoGuess
    <$> SQL.field
    <*> SQL.field
    <*> SQL.field
    <*> SQL.field
    <*> SQL.field
    <*> SQL.field
    <*> SQL.field

data Message = Message
  { messageId :: Int
  , messageMatchId :: Int
  , messageSenderId :: UserID
  , messageContent :: T.Text
  , messageSentOn :: Integer
  , messageIsRead :: Bool
  } deriving (Show, Eq)

instance SQL.FromRow Message where
  fromRow = Message
    <$> SQL.field
    <*> SQL.field
    <*> SQL.field
    <*> SQL.field
    <*> SQL.field
    <*> SQL.field

data AnswerToken = AnswerToken
  { tokenUserId :: UserID
  , tokenAorbId :: AorbID
  , tokenExpiry :: Integer
  } deriving (Show)

data SubmitAnswer = SubmitAnswer
  { submitAorbId :: AorbID
  , submitChoice :: Word.Word8
  , submitToken :: T.Text
  } deriving (Show)

instance JSON.FromJSON SubmitAnswer where
  parseJSON = JSON.withObject "SubmitAnswer" $ \v -> SubmitAnswer
    <$> v JSON..: "aorb_id"
    <*> v JSON..: "choice"
    <*> v JSON..: "token"
