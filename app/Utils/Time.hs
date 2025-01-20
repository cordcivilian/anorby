{-# LANGUAGE OverloadedStrings #-}

module Utils.Time
  ( getTimeUntilNextMidnight
  ) where

import qualified Data.Text as T
import qualified Data.Time.Clock as Clock
import qualified Data.Time.LocalTime as LocalTime
import qualified Data.Time.Calendar as Calendar

-- | Calculate time until next midnight, formatted as "X hours" or "Y minutes"
getTimeUntilNextMidnight :: IO T.Text
getTimeUntilNextMidnight = do
  now <- Clock.getCurrentTime
  let utcTime = LocalTime.utcToLocalTime LocalTime.utc now
      currentDay = LocalTime.localDay utcTime
      nextDay = Calendar.addDays 1 currentDay
      nextMidnight = LocalTime.LocalTime nextDay (LocalTime.TimeOfDay 0 0 0)
      nextMidnightUTC = LocalTime.localTimeToUTC LocalTime.utc nextMidnight
      diffSeconds = round $ Clock.diffUTCTime nextMidnightUTC now
      hours = div diffSeconds 3600
      minutes = div (rem diffSeconds 3600) 60
      roundedHours = if minutes > 30 then hours + 1 else hours :: Int
  return $ if roundedHours > 0
    then T.pack $ show roundedHours <> " hours"
    else T.pack $ show minutes <> " minutes"
