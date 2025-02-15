{-# LANGUAGE OverloadedStrings #-}

module Utils.Cache where

import qualified Control.Concurrent.MVar as MVar
import qualified Data.Map as Map
import qualified Data.Time.Clock as Time
import qualified Data.Time.Clock.POSIX as POSIXTime

data CacheEntry a = CacheEntry
  { entryData :: a
  , entryTimestamp :: POSIXTime.POSIXTime
  , entryTTL :: Time.NominalDiffTime
  }

data Cache a = Cache
  { cacheData :: Map.Map String (CacheEntry a)
  , cacheDuration :: Time.NominalDiffTime
  }

initCache :: Time.NominalDiffTime -> IO (MVar.MVar (Cache a))
initCache defaultTTL = MVar.newMVar Cache
  { cacheData = Map.empty
  , cacheDuration = defaultTTL
  }

getFromCache :: String -> MVar.MVar (Cache a) -> IO (Maybe a)
getFromCache key cacheVar = do
  now <- POSIXTime.getPOSIXTime
  cache <- MVar.readMVar cacheVar
  return $ do
    entry <- Map.lookup key (cacheData cache)
    let age = now - entryTimestamp entry
    if age < cacheDuration cache
      then Just (entryData entry)
      else Nothing

putInCacheWithTTL :: String -> a -> Time.NominalDiffTime -> MVar.MVar (Cache a) -> IO ()
putInCacheWithTTL key value ttl cacheVar = do
  now <- POSIXTime.getPOSIXTime
  MVar.modifyMVar_ cacheVar $ \cache -> return $ cache
    { cacheData = Map.insert key (CacheEntry value now ttl) (cacheData cache)
    }
