module Utils.MatchState where

import qualified Control.Concurrent.MVar as MVar
import qualified Control.Exception as Exception
import qualified Data.Time.Clock.POSIX as POSIXTime

data MatchStatus
  = NotStarted
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
  let yesterday = now - 24 * 60 * 60
  statusVar <- MVar.newMVar NotStarted
  timeVar <- MVar.newMVar yesterday
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
withMatchLock state action =
  MVar.modifyMVar (matchMVar state) $ \status -> case status of
    NotStarted -> do
      Exception.catch
        (do result <- action
            now <- POSIXTime.getPOSIXTime
            MVar.modifyMVar_ (lastMatchedOn state) $ \_ -> return now
            return (Completed, Just result))
        (\e -> do
            let _ = e :: Exception.SomeException
            return (Failed "matching failed", Nothing))
    _ -> return (status, Nothing)
