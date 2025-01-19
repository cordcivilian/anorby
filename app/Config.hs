{-# LANGUAGE OverloadedStrings #-}

module Config where

import qualified System.Environment as Env
import qualified System.Exit as Exit
import qualified Data.Maybe as Maybe
import qualified Control.Monad as Monad

data Environment = Production | TestWithAnswers | TestWithoutAnswers
  deriving (Show, Eq)

data Config = Config
  { environment :: Environment
  , dbPath :: FilePath
  , userCount :: Int
  , newDb :: Bool
  } deriving (Show)

productionDbPath :: FilePath
productionDbPath = "data/anorby.db"

testWithAnswersDbPath :: FilePath
testWithAnswersDbPath = "data/anorby-test-ans.db"

testWithoutAnswersDbPath :: FilePath
testWithoutAnswersDbPath = "data/anorby-test.db"

testOverrideDbPath :: FilePath
testOverrideDbPath = "data/mock-anorby-20250118114756.db"

getEnvironment :: IO Environment
getEnvironment = do
  secret <- Env.lookupEnv "ANORBY"
  withAnswers <- Env.lookupEnv "ANS"
  return $ case (secret, withAnswers) of
    (Just _, _) -> Production 
    (Nothing, Just "1") -> TestWithAnswers
    (Nothing, _) -> TestWithoutAnswers

getConfig :: IO Config
getConfig = do
  env <- getEnvironment
  newFlag <- Maybe.isJust <$> Env.lookupEnv "NEW"
  xDbFlag <- Maybe.isJust <$> Env.lookupEnv "XDB"
  
  Monad.when (env == Production && newFlag) $ do
    putStrLn "ERROR: NEW=1 cannot be used in production"
    Exit.exitWith (Exit.ExitFailure 1)
    
  Monad.when (env == Production && xDbFlag) $ do
    putStrLn "ERROR: XDB=1 cannot be used in production"
    Exit.exitWith (Exit.ExitFailure 1)
  
  let dbPath' = case (env, xDbFlag) of
        (Production, _) -> productionDbPath
        (_, True) -> testOverrideDbPath
        (TestWithAnswers, False) -> testWithAnswersDbPath
        (TestWithoutAnswers, False) -> testWithoutAnswersDbPath
    
  return $ Config 
    { environment = env
    , dbPath = dbPath'
    , userCount = 1337
    , newDb = newFlag
    }
