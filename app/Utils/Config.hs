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
                 | TestWithMatches Int
                 deriving (Show, Eq)

data Config = Config
  { environment :: Environment
  , dbPath :: FilePath
  , userCount :: Int
  , newDb :: Bool
  , matchThreshold :: Int
  , profileThreshold :: Int
  } deriving (Show)

data EnvFlags = EnvFlags
  { hasANORBY :: Bool
  , hasANS :: Bool
  , hasXDB :: Bool
  , hasMCH :: Maybe String
  , hasNEW :: Bool
  , hasSMTP :: Bool
  } deriving (Show)

productionDbPath :: FilePath
productionDbPath = "data/anorby.db"

testWithAnswersDbPath :: FilePath
testWithAnswersDbPath = "data/anorby-test-ans.db"

testWithoutAnswersDbPath :: FilePath
testWithoutAnswersDbPath = "data/anorby-test.db"

testOverrideDbPath :: FilePath
testOverrideDbPath = "data/mock-anorby-20250118114756.db"

testWithMatchesDbPath :: Int -> FilePath
testWithMatchesDbPath n = "data/anorby-test-mch" ++ show n ++ ".db"

-- | Environment validation

getEnvironment :: IO (Environment, Bool)
getEnvironment = do
  flags <- getEnvFlags
  validateEnvCombination flags
  env <- determineEnvironment flags
  return (env, hasNEW flags)

getEnvFlags :: IO EnvFlags
getEnvFlags = do
  secret <- Env.lookupEnv "ANORBY"
  answers <- Env.lookupEnv "ANS"
  override <- Env.lookupEnv "XDB"
  matching <- Env.lookupEnv "MCH"
  new <- Env.lookupEnv "NEW"
  smtp <- Env.lookupEnv "SMTP"
  return $ EnvFlags
    { hasANORBY = Maybe.isJust secret
    , hasANS = Maybe.isJust answers
    , hasXDB = Maybe.isJust override
    , hasMCH = matching
    , hasNEW = Maybe.isJust new
    , hasSMTP = Maybe.isJust smtp
    }

validateEnvCombination :: EnvFlags -> IO ()
validateEnvCombination flags = do
  -- Production incompatibilities
  Monad.when (hasANORBY flags) $ do
    Monad.when (Maybe.isJust $ hasMCH flags) $
      error "MCH cannot be used in production"
    Monad.when (hasNEW flags) $
      error "NEW=1 cannot be used in production"
    Monad.when (hasXDB flags) $
      error "XDB=1 cannot be used in production"
    Monad.when (not $ hasSMTP flags) $
      error "SMTP=1 must be set in production"

determineEnvironment :: EnvFlags -> IO Environment
determineEnvironment flags
  | hasANORBY flags = return Production
  | Maybe.isJust (hasMCH flags) = parseMatchingEnv flags
  | hasXDB flags = return TestWithCustomData
  | hasANS flags = return TestWithAnswers
  | otherwise = return TestWithoutAnswers

parseMatchingEnv :: EnvFlags -> IO Environment
parseMatchingEnv flags = case hasMCH flags of
  Just n -> case (reads n :: [(Int, String)]) of
    [(1, "")] -> return $ TestWithMatches 1
    [(2, "")] -> return $ TestWithMatches 2
    _ -> error "MCH must be 1 (Gale-Shapley) or 2 (Local Search)"
  Nothing -> error "Invalid MCH value"

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
  (env, newFlag) <- getEnvironment

  let dbPath' = case env of
        Production -> productionDbPath
        TestWithCustomData -> testOverrideDbPath
        TestWithAnswers -> testWithAnswersDbPath
        TestWithoutAnswers -> testWithoutAnswersDbPath
        TestWithMatches n -> testWithMatchesDbPath n

  return $ Config
    { environment = env
    , dbPath = dbPath'
    , userCount = 137
    , newDb = newFlag
    , profileThreshold = 10
    , matchThreshold = 20
    }
