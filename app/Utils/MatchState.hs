module Utils.MatchState where

import qualified Control.Concurrent.MVar as MVar
import qualified Data.Time.Clock.POSIX as POSIXTime

data MatchStatus = NotStarted
                 | InProgress
                 | Completed
                 | Failed String
                 deriving (Show, Eq)

data MatchState = MatchState
  { matchMVar :: MVar.MVar MatchStatus
  , lastMatchedOn :: MVar.MVar POSIXTime.POSIXTime
  }

initMatchState :: IO MatchState
initMatchState = do
  now <- POSIXTime.getPOSIXTime
  statusVar <- MVar.newMVar NotStarted
  timeVar <- MVar.newMVar now
  return $ MatchState
    { matchMVar = statusVar
    , lastMatchedOn = timeVar
    }

getMatchStatus :: MatchState -> IO MatchStatus
getMatchStatus state = MVar.readMVar (matchMVar state)

setMatchStatus :: MatchState -> MatchStatus -> IO ()
setMatchStatus state status = do
  MVar.modifyMVar_ (matchMVar state) $ \_ -> return status
  case status of
    Completed -> do
      now <- POSIXTime.getPOSIXTime
      MVar.modifyMVar_ (lastMatchedOn state) $ \_ -> return now
    _ -> return ()

withMatchLock :: MatchState -> IO a -> IO (Maybe a)
withMatchLock state action = do
  acquired <- MVar.tryTakeMVar (matchMVar state)
  case acquired of
    Just NotStarted -> do
      MVar.putMVar (matchMVar state) InProgress
      result <- action
      setMatchStatus state Completed
      return $ Just result
    _ -> return Nothing
