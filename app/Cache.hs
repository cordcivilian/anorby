{-# LANGUAGE OverloadedStrings #-}

module Cache where

import qualified Control.Concurrent.MVar as MVar
import qualified Data.Map as Map
import qualified Data.Time.Clock as Clock

data CacheEntry a = CacheEntry
  { entryData :: a
  , entryTimestamp :: Clock.UTCTime
  }

data Cache a = Cache
  { cacheData :: Map.Map String (CacheEntry a)
  , cacheDuration :: Clock.NominalDiffTime
  }

initCache :: Clock.NominalDiffTime -> IO (MVar.MVar (Cache a))
initCache duration = MVar.newMVar Cache
  { cacheData = Map.empty
  , cacheDuration = duration
  }

getFromCache :: String -> MVar.MVar (Cache a) -> IO (Maybe a)
getFromCache key cacheVar = do
  now <- Clock.getCurrentTime
  cache <- MVar.readMVar cacheVar
  return $ do
    entry <- Map.lookup key (cacheData cache)
    let age = Clock.diffUTCTime now (entryTimestamp entry)
    if age < cacheDuration cache
      then Just (entryData entry)
      else Nothing

putInCache :: String -> a -> MVar.MVar (Cache a) -> IO ()
putInCache key value cacheVar = do
  now <- Clock.getCurrentTime
  MVar.modifyMVar_ cacheVar $ \cache -> return $ cache
    { cacheData = Map.insert key (CacheEntry value now) (cacheData cache)
    }
