{-# LANGUAGE OverloadedStrings #-}

module Utils.Time where

import qualified Data.Text as T
import qualified Data.Time.Clock.POSIX as POSIXTime

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
