{-# LANGUAGE OverloadedStrings #-}
module Utils.MatchTrigger where

import qualified Control.Exception as Exception
import qualified Control.Monad as Monad
import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.MVar as MVar
import qualified Data.Pool as Pool
import qualified Data.Time.Clock.POSIX as POSIXTime
import qualified Data.Time.LocalTime as LocalTime
import qualified Data.Text as T
import qualified Database.SQLite.Simple as SQL

import Core.Ranking
import Core.RollingShadow
import Database
import Utils.Config
import Utils.MatchState
import Web.Types

shouldTriggerMatching :: POSIXTime.POSIXTime -> Config -> Bool
shouldTriggerMatching now config =
  let utcTime = POSIXTime.posixSecondsToUTCTime now
      localTime = LocalTime.utcToLocalTime LocalTime.utc utcTime
      currentTime = LocalTime.localTimeOfDay localTime
      readTimeOfDay :: T.Text -> Maybe LocalTime.TimeOfDay
      readTimeOfDay timeStr =
        case T.splitOn (T.pack ":") timeStr of
          [h, m] -> do
            hour <- readMaybe $ T.unpack h
            minute <- readMaybe $ T.unpack m
            return $ LocalTime.TimeOfDay hour minute 0
          _ -> Nothing
          where
            readMaybe s = case reads s of
              [(x, "")] -> Just x
              _ -> Nothing
      maybeCutoffTime = readTimeOfDay (matchCutoffTime config)
      maybeReleaseTime = readTimeOfDay (matchReleaseTime config)
  in case (maybeCutoffTime, maybeReleaseTime) of
       (Just cutoff, Just release) ->
         currentTime >= cutoff && currentTime < release
       _ -> False

hasAlreadyMatchedToday :: AppState -> POSIXTime.POSIXTime -> IO Bool
hasAlreadyMatchedToday state now = do
    lastMatchedTime <- MVar.readMVar (lastMatchedOn $ appMatchState state)
    let lastMatchDay =
          LocalTime.localDay $ LocalTime.utcToLocalTime LocalTime.utc $
            POSIXTime.posixSecondsToUTCTime lastMatchedTime
        currentDay =
          LocalTime.localDay $ LocalTime.utcToLocalTime LocalTime.utc $
            POSIXTime.posixSecondsToUTCTime now
        matchedInMemory = lastMatchDay == currentDay
    if matchedInMemory then return True else do
      let startOfDay = floor (now / 86400) * 86400 :: Integer
      matchedInDb <- Pool.withResource (appPool state) $ \conn -> do
          matches <- SQL.query conn
              (SQL.Query $
                T.pack "SELECT 1 FROM matched WHERE matched_on >= ? LIMIT 1"
              ) [startOfDay] :: IO [SQL.Only Int]
          return $ not $ null matches
      Monad.when matchedInDb $ do
          MVar.modifyMVar_ (lastMatchedOn $ appMatchState state) $
            \_ -> return now
          MVar.modifyMVar_ (matchMVar $ appMatchState state) $
            \_ -> return Completed
      return matchedInDb

checkAndTriggerMatching :: AppState -> Config -> IO ()
checkAndTriggerMatching state config = do
  now <- POSIXTime.getPOSIXTime
  alreadyMatched <- hasAlreadyMatchedToday state now
  Monad.when (shouldTriggerMatching now config && not alreadyMatched) $ do
    Monad.void $ Concurrent.forkIO $ do
      result <- withMatchLock (appMatchState state) $ do
        Pool.withResource (appPool state) $ \conn -> do
          users <- getUsersWithCompletedAnswers conn
          subs <- allAorbsToSubmissions conn users
          deprioritizationMap <- getRecentMatchMap conn users
          let rankings = submissionsToRankings subs deprioritizationMap
          marriages <- matchWithShadow conn rankings
          insertMatches conn marriages
          Monad.void $ Concurrent.forkIO $ do
            Pool.withResource (appPool state) $ \cleanupConn -> do
              SQL.execute_ cleanupConn "PRAGMA wal_checkpoint(PASSIVE)"
              SQL.execute_ cleanupConn "PRAGMA wal_checkpoint(FULL)"
              SQL.execute_ cleanupConn "PRAGMA optimize"
          cleanupExpiredMessages config conn
          return marriages
      case result of
        Just _ -> Monad.void $ Exception.evaluate result
        Nothing -> return ()
