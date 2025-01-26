{-# LANGUAGE OverloadedStrings #-}

module Utils.Time where

import qualified Data.Text as T
import qualified Data.Time.Clock.POSIX as POSIXTime
import qualified Data.Time.LocalTime as LocalTime
import qualified Data.Time.Clock as Clock

getTimeUntilNextMidnight :: IO T.Text
getTimeUntilNextMidnight = do
  now <- POSIXTime.getPOSIXTime
  let currentSeconds = floor now :: Integer
      secondsInDay = floor POSIXTime.posixDayLength :: Integer
      secondsSinceMidnight = mod currentSeconds secondsInDay
      secondsUntilMidnight = secondsInDay - secondsSinceMidnight
      hours = div secondsUntilMidnight 3600
      minutes = div (mod secondsUntilMidnight 3600) 60
      roundedHours = if minutes > 30 then hours + 1 else hours
  return $ if roundedHours > 0
    then T.pack $ show roundedHours <> " hours"
    else T.pack $ show minutes <> " minutes"

formatTimeUntil :: POSIXTime.POSIXTime -> POSIXTime.POSIXTime -> T.Text
formatTimeUntil now target =
  let diffSeconds = floor (target - now) :: Integer
      hours = div diffSeconds 3600
      minutes = div (mod diffSeconds 3600) 60
      roundedHours = if minutes > 30 then hours + 1 else hours
  in if roundedHours > 0
     then T.pack $ show roundedHours <> " hours"
     else T.pack $ show minutes <> " minutes"

parseMatchTime :: T.Text -> IO (Maybe POSIXTime.POSIXTime)
parseMatchTime timeStr = do
  now <- POSIXTime.getPOSIXTime
  let todayUtc = POSIXTime.posixSecondsToUTCTime now
      today = Clock.utctDay todayUtc
      (hourStr, minStr) = case T.splitOn ":" timeStr of
        [h, m] -> (h, m)
        _ -> (T.empty, T.empty)
      mHour = readMaybe $ T.unpack hourStr
      mMin = readMaybe $ T.unpack minStr
  case (mHour, mMin) of
    (Just hour, Just mins)
      | hour >= 0 && hour < 24 && mins >= 0 && mins < 60 -> do
        let timeOfDay = LocalTime.TimeOfDay hour mins 0
            localTime = LocalTime.LocalTime today timeOfDay
            utcTime = LocalTime.localTimeToUTC LocalTime.utc localTime
        return $ Just $ POSIXTime.utcTimeToPOSIXSeconds utcTime
    _ -> return Nothing
  where
    readMaybe :: Read a => String -> Maybe a
    readMaybe s = case reads s of
      [(x, "")] -> Just x
      _ -> Nothing
