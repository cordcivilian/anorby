module Utils.MatchTrigger where

import qualified Control.Monad as Monad
import qualified Data.Pool as Pool
import qualified Data.Time.Clock.POSIX as POSIXTime
import qualified Data.Time.LocalTime as LocalTime
import qualified Data.Text as T

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

checkAndTriggerMatching :: AppState -> Config -> IO ()
checkAndTriggerMatching state config = do
  now <- POSIXTime.getPOSIXTime
  Monad.when (shouldTriggerMatching now config) $ do
    Monad.void $ withMatchLock (appMatchState state) $ do
      Pool.withResource (appPool state) $ \conn -> do
        users <- getUsersWithCompletedAnswers conn
        subs <- allAorbsToSubmissions conn users
        marriages <- matchWithShadow conn (submissionsToRankings subs)
        insertMatches conn marriages
        return marriages
