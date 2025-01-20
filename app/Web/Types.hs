module Web.Types
  ( AppState(..)
  , Logger
  , MessageTemplate(..)
  , NavLink(..)
  ) where

import qualified Control.Concurrent.MVar as MVar
import qualified Data.Pool as Pool
import qualified Data.Text as T
import qualified Database.SQLite.Simple as SQL

import Types
import Utils.Cache

type Logger = T.Text -> IO ()

data AppState = AppState
  { appPool :: Pool.Pool SQL.Connection
  , appRootCache :: MVar.MVar (Cache [Aorb])
  }

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

