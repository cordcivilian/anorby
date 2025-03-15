module Web.Types where

import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.MVar as MVar
import qualified Data.Pool as Pool
import qualified Data.Text as T
import qualified Database.SQLite.Simple as SQL
import qualified Data.ByteString.Lazy as BSL


import Types
import Utils.Cache
import Utils.MatchState

type Logger = T.Text -> IO ()

data AppState = AppState
  { appPool :: Pool.Pool SQL.Connection
  , appRootCache :: MVar.MVar (Cache [Aorb])
  , appStatsCache :: MVar.MVar (Cache RootStats)
  , appHtmlCache :: MVar.MVar (Cache BSL.ByteString)
  , appQueryCache :: MVar.MVar (Cache QueryResult)
  , appStaticCache :: MVar.MVar (Cache (BSL.ByteString, T.Text))
  , appMatchState :: MatchState
  , appPingTimer :: MVar.MVar (Maybe Concurrent.ThreadId)
  }

data RootStats = RootStats
  { rootTotalQuestions :: Int
  , rootTotalAnswers :: Int
  , rootTodayAnswers :: Int
  , rootActiveUsers :: Int
  , rootNewUsers :: Int
  , rootNewQuestions :: Int
  , rootEnrolledCount :: Int
  } deriving (Show)

data QueryResult = IntResult Int | UserResult [User] | AorbResult [Aorb]
  deriving (Show)

data MessageTemplate = MessageTemplate
  { messageTitle :: T.Text
  , messageHeading :: T.Text
  , messageLink :: (T.Text, T.Text)
  }

data NavLink = NavLink
  { linkPath :: T.Text
  , linkText :: T.Text
  , linkActive :: Bool
  }
