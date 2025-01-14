{-# LANGUAGE OverloadedStrings #-}

module Simulate where

import qualified System.Random as Random

import qualified Data.Time as Time
import qualified Data.Time.Format as DateTimeFormat
import qualified Data.Text as T
import qualified Data.Map as Map

import qualified Text.Printf as Text

import qualified Control.Monad as Monad

import qualified Database.SQLite.Simple as SQL

import Anorby
import Rank
import Marry

randomBinaryVector :: Int -> Int -> BinaryVector
randomBinaryVector seed len =
  take len $ randomRs (AorbAnswer 0, AorbAnswer 1) $ Random.mkStdGen seed

randomRs :: (Random.Random a, Random.RandomGen g) => (a, a) -> g -> [a]
randomRs range = unfoldr (Just . Random.randomR range)
  where
    unfoldr f b =
      case f b of
        Just (a, b') -> a : unfoldr f b'
        Nothing      -> []

-- ---------------------------------------------------------------------------

generateSubmissions :: Int -> Int -> Int -> Submissions
generateSubmissions n len seed = do
  let submissionsList = map (generateSubmission len) [seed .. (seed + n - 1)]
  Map.fromList $ zip [0 ..] submissionsList

generateSubmission :: Int -> Int -> UserSubmission
generateSubmission len seed =
  let binaryVector = randomBinaryVector seed len
      rng = Random.mkStdGen (seed + 1)
      (weightIndex, _) = Random.randomR (0, len - 1) rng
      schemes = [PPPod, Balance, Bipolar]
      schemeIndex = rem seed 3
      scheme = schemes !! schemeIndex
  in (binaryVector, weightIndex, scheme)

testSubmissionsToRankings :: Int -> Int -> Int -> Rankings
testSubmissionsToRankings n len seed =
  submissionsToRankings $ generateSubmissions n len seed

-- ---------------------------------------------------------------------------

randomRankings :: (Random.RandomGen g) => g -> Int -> Rankings
randomRankings gen n =
  Map.fromList $ zip [1 .. n] (manyRankings gen n)
  where
    manyRankings :: (Random.RandomGen g) => g -> Int -> [Ranking]
    manyRankings g size =
      let (preferences, _) = foldl (randomRanking size) ([], g) [1 .. size]
      in reverse preferences
    randomRanking :: (Random.RandomGen g)
                  => Int -> ([Ranking], g) -> Int
                  -> ([Ranking], g)
    randomRanking size (prefs, g) person =
      let others = filter (/= person) [1 .. size]
          (shuffled, newGen) = fisherYatesShuffle g others
      in (shuffled : prefs, newGen)

-- ---------------------------------------------------------------------------

mockBaseAorbAnswers :: SQL.Connection -> Int -> IO ()
mockBaseAorbAnswers conn n = do
  putStrLn $ "Initializing tables for " ++ show n ++ " users..."
  initTables conn
  putStrLn "Generating mock users..."
  mockUsers conn n
  putStrLn "Ingesting base A/B data..."
  ingestBaseAorbData conn
  putStrLn "Fetching generated users..."
  users <- SQL.query_ conn "SELECT * FROM users" :: IO [User]
  putStrLn $ "Found " ++ show (length users) ++ " users"
  putStrLn "Fetching A/B items..."
  aorbs <- SQL.query_ conn "SELECT * FROM aorb" :: IO [Aorb]
  putStrLn $ "Found " ++ show (length aorbs) ++ " A/B items"
  putStrLn "Generating mock A/B answers..."
  mockAorbAnswers conn aorbs users
  putStrLn "Mock data generation complete."

mockUsers :: SQL.Connection -> Int -> IO ()
mockUsers conn n = do
  putStrLn $ "Getting random seed for " ++ show n ++ " users..."
  gen <- Random.getStdGen
  putStrLn "Generating mock user data..."
  (users, _) <- generateMockUsers n gen
  putStrLn $ "Inserting " ++ show (length users) ++ " users into database..."
  SQL.withTransaction conn $ SQL.executeMany conn
    "INSERT OR REPLACE INTO users (id, name, email, aorb_id, assoc) VALUES (?, ?, ?, ?, ?)"
    users
  putStrLn "User generation complete."
  where
    generateMockUsers :: Int -> Random.StdGen -> IO ([User], Random.StdGen)
    generateMockUsers count g = do
      putStrLn "Starting user generation loop..."
      Monad.foldM (\(accUsers, currGen) i -> do
        let (mockAssoc, g2) = Random.random currGen
            (mockAorbId, g3) = Random.randomR (1, 100) g2
            user = User
              { userId = i + 1
              , userName = T.pack $ "User" ++ show (i + 1)
              , userEmail = T.pack $ "user" ++ show (i + 1) ++ "@example.com"
              , userAorbId = mockAorbId
              , userAssoc = mockAssoc
              }
        Monad.when (rem i 100 == 0) $
          putStrLn $ "Generated " ++ show i ++ " users..."
        return (user : accUsers, g3)
        ) ([], g) [0..count-1]

mockAorbAnswers :: SQL.Connection -> [Aorb] -> [User] -> IO ()
mockAorbAnswers conn aorbs users = do
  putStrLn $
    "Getting random seed for "
    ++ show (length users)
    ++ " users × "
    ++ show (length aorbs)
    ++ " A/B items..."
  gen <- Random.getStdGen
  putStrLn "Generating mock answers..."
  (answers, _) <- generateMockAnswers gen
  putStrLn $
    "Inserting "
    ++ show (length answers)
    ++ " answers into database..."
  SQL.withTransaction conn $ SQL.executeMany conn
    "INSERT OR REPLACE INTO aorb_answers (user_id, aorb_id, answer) VALUES (?, ?, ?)"
    answers
  putStrLn "Answer generation complete."
  where
    generateMockAnswers :: Random.StdGen -> IO ([AorbAnswers], Random.StdGen)
    generateMockAnswers g = do
      putStrLn "Starting answer generation loop..."
      let total = length users * length aorbs
      Monad.foldM (\(accAnswers, currGen) (idx, (u, a)) -> do
        let (answer, nextGen) = Random.random currGen
            mockAorbAnswer = AorbAnswers
              { aorbUserId = userId u
              , aorbAorbId = aorbId a
              , aorbAnswer = answer
              }
        Monad.when (rem idx (1000 :: Int) == 0) $
          putStrLn $
            "Generated "
            ++ show idx
            ++ "/"
            ++ show total
            ++ " answers..."
        return (mockAorbAnswer : accAnswers, nextGen)
        ) ([], g) $ zip [0..] [(u, a) | u <- users, a <- aorbs]

testUserTopAorbs :: Int -> IO ()
testUserTopAorbs n = do

  now <- Time.getCurrentTime
  let timestamp = DateTimeFormat.formatTime
                  DateTimeFormat.defaultTimeLocale
                  "%Y%m%d%H%M%S"
                  now
      dbName = "data/test-anorby-" ++ timestamp ++ ".db"

  putStrLn $ "Creating test database: " ++ dbName
  conn <- initDB dbName

  putStrLn $ "\nGenerating mock data for " ++ show n ++ " users..."
  mockBaseAorbAnswers conn n

  putStrLn "Most commonplace A/B choices:"
  common <- getUserTopXMostCommonplace conn 1 3
  mapM_ (\awa -> do
    let aorb = aorbData awa
        answer = case userAnswer awa of
          AorbAnswer 0 -> "A: " ++ T.unpack (aorbA aorb)
          AorbAnswer _ -> "B: " ++ T.unpack (aorbB aorb)
        percentageText = case userAnswer awa of
          AorbAnswer 0 ->
            let percentB = aorbMean aorb * 100
                percentA = 100 - percentB
            in Text.printf "%.1f%% of users also chose A" percentA
          AorbAnswer 1 ->
            let percentB = aorbMean aorb * 100
            in Text.printf "%.1f%% of users also chose B" percentB
          _ -> "unknown"
    putStrLn $ "  ID: " ++ show (aorbId aorb)
    putStrLn $ "  Context: " ++ T.unpack (aorbCtx aorb)
    putStrLn $ "  Subtext: " ++ T.unpack (aorbStx aorb)
    putStrLn $ "  A: " ++ T.unpack (aorbA aorb)
    putStrLn $ "  B: " ++ T.unpack (aorbB aorb)
    putStrLn $ "  Mean: " ++ show (aorbMean aorb)
    putStrLn $ "  User chose: " ++ answer
    putStrLn $ "  Agreement: " ++ percentageText
    putStrLn ""
    ) common

  putStrLn "Most controversial A/B choices:"
  controversial <- getUserTopXMostControversial conn 1 3
  mapM_ (\awa -> do
    let aorb = aorbData awa
        answer = case userAnswer awa of
          AorbAnswer 0 -> "A: " ++ T.unpack (aorbA aorb)
          AorbAnswer _ -> "B: " ++ T.unpack (aorbB aorb)
        percentageText = case userAnswer awa of
          AorbAnswer 0 ->
            let percentB = aorbMean aorb * 100
                percentA = 100 - percentB
            in Text.printf "%.1f%% of users also chose A" percentA
          AorbAnswer 1 ->
            let percentB = aorbMean aorb * 100
            in Text.printf "%.1f%% of users also chose B" percentB
          _ -> "unknown"
    putStrLn $ "  ID: " ++ show (aorbId aorb)
    putStrLn $ "  Context: " ++ T.unpack (aorbCtx aorb)
    putStrLn $ "  Subtext: " ++ T.unpack (aorbStx aorb)
    putStrLn $ "  A: " ++ T.unpack (aorbA aorb)
    putStrLn $ "  B: " ++ T.unpack (aorbB aorb)
    putStrLn $ "  Mean: " ++ show (aorbMean aorb)
    putStrLn $ "  User chose: " ++ answer
    putStrLn $ "  Agreement: " ++ percentageText
    putStrLn ""
    ) controversial

  putStrLn "Closing database connection..."
  SQL.close conn
  putStrLn "Test complete."

-- ---------------------------------------------------------------------------

testAorbGaleShapley :: Int -> IO ()
testAorbGaleShapley n
  | n <= 1 = putStrLn "number of matching entities must be greater than two"
  | otherwise = do
  now <- Time.getCurrentTime
  let timestamp = DateTimeFormat.formatTime
                  DateTimeFormat.defaultTimeLocale
                  "%Y%m%d%H%M%S"
                  now
      dbName = "data/test-anorby-" ++ timestamp ++ ".db"
  conn <- initDB dbName
  mockBaseAorbAnswers conn n
  submissions <- baseAorbsToSubmissions conn
  let rankings = submissionsToRankings submissions
  testGaleShapleyFromRankings rankings
  SQL.close conn

testRandomSubmissionsGaleShapley :: Int -> Int -> Int -> IO ()
testRandomSubmissionsGaleShapley n len seed
  | n <= 1 = putStrLn "number of matching entities must be greater than two"
  | len <= 0 = putStrLn "vector size must be at least one"
  | otherwise =
  let submissions = generateSubmissions n len seed
  in testGaleShapleyFromRankings $ submissionsToRankings submissions

testRandomRankingsGaleShapley :: Int -> IO ()
testRandomRankingsGaleShapley n
  | n <= 1 = putStrLn "number of matching entities must be greater than two"
  | otherwise = do
  gen <- Random.getStdGen
  let rankings = randomRankings gen n
  testGaleShapleyFromRankings rankings

-- ---------------------------------------------------------------------------

testAorbLocalSearch :: Int -> Int -> Double -> Int -> IO ()
testAorbLocalSearch n maxIterations maxBlockingPercentage batchSize
  | n <= 1 = putStrLn "number of matching entities must be greater than two"
  | maxIterations <= 0 = putStrLn "max iterations must be positive"
  | maxBlockingPercentage <= 0.0 = putStrLn "max blocking % must be positive"
  | batchSize <= 1 = putStrLn "batch size must be at least one"
  | otherwise = do
  now <- Time.getCurrentTime
  let timestamp = DateTimeFormat.formatTime
                  DateTimeFormat.defaultTimeLocale
                  "%Y%m%d%H%M%S"
                  now
      dbName = "data/test-anorby-" ++ timestamp ++ ".db"
  conn <- initDB dbName
  mockBaseAorbAnswers conn n
  submissions <- baseAorbsToSubmissions conn
  let rankings = submissionsToRankings submissions
  testLocalSearchFromRankings
    rankings maxIterations maxBlockingPercentage batchSize
  SQL.close conn

testRandomSubmissionsLocalSearch :: Int -> Int -> Int -> Int -> Double -> Int
                                -> IO ()
testRandomSubmissionsLocalSearch
  n len seed maxIterations maxBlockingPercentage batchSize
  | n <= 1 = putStrLn "number of matching entities must be greater than two"
  | len <= 0 = putStrLn "vector size must be at least one"
  | maxIterations <= 0 = putStrLn "max iterations must be positive"
  | maxBlockingPercentage <= 0.0 = putStrLn "max blocking % must be positive"
  | batchSize <= 1 = putStrLn "batch size must be at least one"
  | otherwise =
  let submissions = generateSubmissions n len seed
  in testLocalSearchFromRankings
      (submissionsToRankings submissions)
      maxIterations maxBlockingPercentage batchSize

testRandomRankingsLocalSearch :: Int -> Int -> Double -> Int -> IO ()
testRandomRankingsLocalSearch
  n maxIterations maxBlockingPercentage batchSize
  | n <= 1 = putStrLn "number of matching entities must be greater than two"
  | maxIterations <= 0 = putStrLn "max iterations must be positive"
  | maxBlockingPercentage <= 0.0 = putStrLn "max blocking % must be positive"
  | batchSize <= 1 = putStrLn "batch size must be at least one"
  | otherwise = do
  gen <- Random.getStdGen
  let rankings = randomRankings gen n
  testLocalSearchFromRankings
    rankings maxIterations maxBlockingPercentage batchSize

-- ---------------------------------------------------------------------------

testGaleShapleyFromRankings :: Rankings -> IO ()
testGaleShapleyFromRankings rankings = do
  putStrLn "Complete Rankings:"
  mapM_
    (\(uid, ranking) -> putStrLn $ show uid ++ ": " ++ show ranking)
    (Map.toList rankings)

  let (group1Rankings, group2Rankings) = splitRankings rankings
  putStrLn "\nGroup 1 Rankings:"
  mapM_
    (\(uid, ranking) -> putStrLn $ show uid ++ ": " ++ show ranking)
    (Map.toList group1Rankings)
  putStrLn "\nGroup 2 Rankings:"
  mapM_
    (\(uid, ranking) -> putStrLn $ show uid ++ ": " ++ show ranking)
    (Map.toList group2Rankings)

  marriages <- galeShapley group1Rankings group2Rankings
  putStrLn "\nFinal Marriages:"
  mapM_
    (\(uid, partner) -> putStrLn $ show uid ++ " -> " ++ show partner)
    (Map.toList marriages)

  let stable = isStableMarriage group1Rankings group2Rankings marriages
  putStrLn $ "\nMatching is stable: " ++ show stable

testLocalSearchFromRankings :: Rankings -> Int -> Double -> Int -> IO ()
testLocalSearchFromRankings
  rankings maxIterations maxBlockingPercentage batchSize = do
  putStrLn "Complete Rankings:"
  mapM_
    (\(uid, ranking) -> putStrLn $ show uid ++ ": " ++ show ranking)
    (Map.toList rankings)

  let (group1Rankings, group2Rankings) = splitRankings rankings
  putStrLn "\nGroup 1 Rankings:"
  mapM_
    (\(uid, ranking) -> putStrLn $ show uid ++ ": " ++ show ranking)
    (Map.toList group1Rankings)
  putStrLn "\nGroup 2 Rankings:"
  mapM_
    (\(uid, ranking) -> putStrLn $ show uid ++ ": " ++ show ranking)
    (Map.toList group2Rankings)

  marriages <-
    localSearch group1Rankings group2Rankings
      maxIterations maxBlockingPercentage batchSize
  putStrLn "\nFinal Marriages:"
  mapM_
    (\(uid, partner) -> putStrLn $ show uid ++ " -> " ++ show partner)
    (Map.toList marriages)

  let stable = isStableMarriage group1Rankings group2Rankings marriages
  putStrLn $ "\nMatching is stable: " ++ show stable

  let finalPercentage =
        blockingPairsPercentage group1Rankings group2Rankings marriages
  putStrLn $ "Final blocking pairs percentage: " ++ show finalPercentage ++ "%"
