{-# LANGUAGE OverloadedStrings #-}

module Utils.Config where

import qualified System.Environment as Env
import qualified System.Exit as Exit
import qualified Data.Maybe as Maybe
import qualified Control.Monad as Monad
import qualified System.Directory as Dir

data Environment = Production
                 | TestWithAnswers
                 | TestWithoutAnswers
                 | TestWithCustomData
  deriving (Show, Eq)

data Config = Config
  { environment :: Environment
  , dbPath :: FilePath
  , userCount :: Int
  , newDb :: Bool
  , matchThreshold :: Int
  , profileThreshold :: Int
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
  overrideDb <- Env.lookupEnv "XDB"
  return $ case (secret, withAnswers, overrideDb) of
    (Just _, _, _) -> Production
    (Nothing, _, Just "1") -> TestWithCustomData
    (Nothing, Just "1", _) -> TestWithAnswers
    (Nothing, _, _) -> TestWithoutAnswers

checkRequiredFiles :: IO ()
checkRequiredFiles = do
  dataExists <- Dir.doesDirectoryExist "data"
  baseJsonExists <- Dir.doesFileExist "data/base.json"

  Monad.when (not dataExists) $ do
    putStrLn "ERROR: /data directory is missing"
    Exit.exitWith (Exit.ExitFailure 1)

  Monad.when (not baseJsonExists) $ do
    putStrLn "ERROR: base aorbs file is missing"
    Exit.exitWith (Exit.ExitFailure 1)

getConfig :: IO Config
getConfig = do
  checkRequiredFiles

  env <- getEnvironment
  newFlag <- Maybe.isJust <$> Env.lookupEnv "NEW"
  xDbFlag <- Maybe.isJust <$> Env.lookupEnv "XDB"
  smtpFlag <- Maybe.isJust <$> Env.lookupEnv "SMTP"

  Monad.when (env == Production && newFlag) $ do
    putStrLn "ERROR: NEW=1 cannot be used in production"
    Exit.exitWith (Exit.ExitFailure 1)

  Monad.when (env == Production && xDbFlag) $ do
    putStrLn "ERROR: XDB=1 cannot be used in production"
    Exit.exitWith (Exit.ExitFailure 1)

  Monad.when (env == Production && not smtpFlag) $ do
    putStrLn "ERROR: SMTP=1 must be set in production"
    Exit.exitWith (Exit.ExitFailure 1)

  let dbPath' = case env of
                  Production -> productionDbPath
                  TestWithCustomData -> testOverrideDbPath
                  TestWithAnswers -> testWithAnswersDbPath
                  TestWithoutAnswers -> testWithoutAnswersDbPath

  return $ Config
    { environment = env
    , dbPath = dbPath'
    , userCount = 1337
    , newDb = newFlag
    , profileThreshold = 10
    , matchThreshold = 20
    }
