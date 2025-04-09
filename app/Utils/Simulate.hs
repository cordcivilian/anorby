 {-# LANGUAGE OverloadedStrings #-}

module Utils.Simulate where

import qualified Control.Monad as Monad
import qualified Data.Map as Map
import qualified Data.Map.Strict as StrictMap
import qualified Data.Text as T
import qualified Data.Time.Clock.POSIX as POSIXTime
import qualified Data.Time.Format as DateTimeFormat
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import qualified Database.SQLite.Simple as SQL
import qualified System.Random as Random
import qualified Text.Printf as Text

import Types
import Database
import Core.Matching
import Core.Ranking
import Core.RollingShadow
import Utils.Config

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

-- | Mock Data Generation

mockery :: Int -> Environment -> IO ()
mockery n env = do
  conn <- mockDB "mock"
  case env of
    TestWithAnswers -> mockBaseAorbAnswers conn n
    TestWithoutAnswers -> mockBase conn n
    TestWithMatches 1 -> mockBaseAorbAnswersWithGaleShapley conn n
    TestWithMatches 2 -> mockBaseAorbAnswersWithLocalSearch conn n
    _ -> mockBase conn n
  SQL.close conn

mockDB :: FilePath -> IO SQL.Connection
mockDB prefix = do
  now <- POSIXTime.getPOSIXTime
  let timestamp = DateTimeFormat.formatTime
                  DateTimeFormat.defaultTimeLocale
                  "%Y%m%d%H%M%S"
                  (POSIXTime.posixSecondsToUTCTime now)
      dbName = "data/" ++ prefix ++ "-anorby-" ++ timestamp ++ ".db"
  putStrLn $ "Creating test database: " ++ dbName
  initDB dbName False

mockBase :: SQL.Connection -> Int -> IO ()
mockBase conn n = do
  putStrLn $ "Initializing tables for " ++ show n ++ " users..."
  initTables conn
  putStrLn "Generating mock users..."
  mockUsers conn n
  putStrLn "Mock base generation complete."

mockBaseAorbAnswers :: SQL.Connection -> Int -> IO ()
mockBaseAorbAnswers conn n = do
  mockBase conn n
  putStrLn "Fetching generated users..."
  users <- SQL.query_ conn "SELECT * FROM users" :: IO [User]
  putStrLn $ "Found " ++ show (length users) ++ " users"
  putStrLn "Fetching A/B items..."
  aorbs <- SQL.query_ conn "SELECT * FROM aorb" :: IO [Aorb]
  putStrLn $ "Found " ++ show (length aorbs) ++ " A/B items"
  putStrLn "Generating mock A/B answers..."
  mockAorbAnswers conn aorbs users
  putStrLn "Setting main aorbs and association schemes..."
  mockMainAorbs conn users
  mockAssociations conn users
  putStrLn "Mock data generation complete."
  ensureShadowUser conn
  putStrLn "Shadow user created."

type MatchingAlgorithm = Rankings -> Rankings -> IO Marriages

mockBaseAorbAnswersWithMatching :: SQL.Connection -> Int -> MatchingAlgorithm -> IO ()
mockBaseAorbAnswersWithMatching conn n matchingAlgorithm = do
  mockBaseAorbAnswers conn n
  putStrLn "Running matching backfill..."

  now <- POSIXTime.getPOSIXTime
  let currentTimestamp = floor now :: Integer
      secondsPerDay = 86400
      backfillDays = 14

  Monad.forM_ [0..backfillDays-1] $ \daysAgo -> do
    let dayTimestamp = currentTimestamp - (daysAgo * secondsPerDay)
        startOfDay = (div dayTimestamp secondsPerDay) * secondsPerDay

    putStrLn $ "Generating matches for " ++ show daysAgo ++ " days ago..."

    submissions <- baseAorbsToSubmissions conn
    users <- getUsersWithCompletedAnswers conn
    deprioritizationMap <- getRecentMatchMap conn users
    let rankings = submissionsToRankings submissions deprioritizationMap
    rankingsWithShadow <- addShadowUserIfNeeded conn rankings
    let (group1Rankings, group2Rankings) = splitRankings rankingsWithShadow
    marriages <- matchingAlgorithm group1Rankings group2Rankings

    let matches = marriagesToDatedMatches (fromIntegral startOfDay) marriages
    SQL.withTransaction conn $
      SQL.executeMany conn
        (SQL.Query $ T.unwords
          [ "INSERT INTO matched"
          , "(user_id, target_id, matched_on)"
          , "VALUES (?, ?, ?)"
          ]
        ) matches

    updateUnmatchedStatus conn marriages
    putStrLn $ "Completed matches for day " ++ show daysAgo

  putStrLn "Mock data with matching history complete."

marriagesToDatedMatches :: POSIXTime.POSIXTime -> Marriages -> [Match]
marriagesToDatedMatches timestamp marriages =
  [ Match uid tid (floor timestamp)
    | (uid, Just tid) <- StrictMap.toList marriages
    , uid < tid
  ] >>= makeSymmetric
   where
    makeSymmetric match =
      [ match
      , match { matchUserId = matchTargetId match
              , matchTargetId = matchUserId match
              }
      ]

localSearchWithParams :: Rankings -> Rankings -> IO Marriages
localSearchWithParams = \g1 g2 -> localSearch g1 g2 1000 1.0 5

mockBaseAorbAnswersWithGaleShapley :: SQL.Connection -> Int -> IO ()
mockBaseAorbAnswersWithGaleShapley conn n =
  mockBaseAorbAnswersWithMatching conn n galeShapley

mockBaseAorbAnswersWithLocalSearch :: SQL.Connection -> Int -> IO ()
mockBaseAorbAnswersWithLocalSearch conn n =
  mockBaseAorbAnswersWithMatching conn n localSearchWithParams

mockUsers :: SQL.Connection -> Int -> IO ()
mockUsers conn n = do
  putStrLn $ "Getting random seed for " ++ show n ++ " users..."
  gen <- Random.getStdGen
  putStrLn "Generating mock user data..."
  (users, _) <- generateMockUsers n gen
  putStrLn $ "Inserting " ++ show (length users) ++ " users into database..."
  SQL.withTransaction conn $ SQL.executeMany conn
    (SQL.Query $ T.unwords
      [ "INSERT OR REPLACE INTO"
      , "users (id, name, email, uuid, aorb_id, assoc)"
      , "VALUES (?, ?, ?, ?, ?, ?)"
      ]
    ) users
  putStrLn "User generation complete."
  where
    generateMockUsers :: Int -> Random.StdGen -> IO ([User], Random.StdGen)
    generateMockUsers count g = do
      putStrLn "Starting user generation loop..."
      Monad.foldM (\(accUsers, currGen) i -> do
        mockUuid <- UUID.toString <$> UUID.nextRandom
        let user = User
              { userId = i + 1
              , userName = T.pack $ "User" ++ show (i + 1)
              , userEmail = T.pack $ "user" ++ show (i + 1) ++ "@example.com"
              , userUuid = T.pack mockUuid
              , userAorbId = Nothing
              , userAssoc = Nothing
              , userCreatedOn = 0
              }
        Monad.when (rem i 100 == 0) $
          putStrLn $ "Generated " ++ show i ++ " users..."
        return (user : accUsers, currGen)
        ) ([], g) [0..count-1]

mockMainAorbs :: SQL.Connection -> [User] -> IO ()
mockMainAorbs conn users =
  Monad.forM_ users $ \user -> do
    answeredAorbs <- SQL.query conn
      "SELECT DISTINCT aorb_id FROM aorb_answers WHERE user_id = ?"
      (SQL.Only $ userId user) :: IO [SQL.Only Int]
    Monad.unless (null answeredAorbs) $ do
      let gen = Random.mkStdGen (userId user)
          (idx, _) = Random.randomR (0, length answeredAorbs - 1) gen
          SQL.Only favouriteId = answeredAorbs !! idx
      SQL.execute conn
        "UPDATE users SET aorb_id = ? WHERE id = ?"
        (favouriteId, userId user)

mockAssociations :: SQL.Connection -> [User] -> IO ()
mockAssociations conn users =
  Monad.forM_ users $ \user -> do
    let gen = Random.mkStdGen (userId user)
        (assocScheme, _) = Random.random gen
    SQL.execute conn
      "UPDATE users SET assoc = ? WHERE id = ?"
      (assocScheme :: AssociationScheme, userId user)

mockAorbAnswers :: SQL.Connection -> [Aorb] -> [User] -> IO ()
mockAorbAnswers conn aorbs users = do
  putStrLn $
    "Getting random seed for "
    ++ show (length users)
    ++ " users × "
    ++ show (length aorbs)
    ++ " A/B items..."
  gen <- Random.getStdGen
  now <- POSIXTime.getPOSIXTime
  putStrLn "Generating mock answers..."
  (answers, _) <- generateMockAnswers gen now
  putStrLn $
    "Inserting "
    ++ show (length answers)
    ++ " answers into database..."
  SQL.withTransaction conn $ SQL.executeMany conn
    (SQL.Query $ T.unwords
      [ "INSERT OR REPLACE INTO aorb_answers"
      , "(user_id, aorb_id, answer, answered_on)"
      , "VALUES (?, ?, ?, ?)"
      ]
    ) answers
  putStrLn "Answer generation complete."
  where
    generateMockAnswers :: Random.StdGen -> POSIXTime.POSIXTime
                        -> IO ([AorbAnswers], Random.StdGen)
    generateMockAnswers g currentTime = do
      putStrLn "Starting answer generation loop..."
      let total = length users * length aorbs
      Monad.foldM (\(accAnswers, currGen) (idx, (u, a)) -> do
        let (answer, nextGen) = Random.random currGen
            mockAorbAnswer = AorbAnswers
              { aorbUserId = userId u
              , aorbAorbId = aorbId a
              , aorbAnswer = answer
              , aorbAnsweredOn = floor currentTime
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

-- | Testing Functions

testUserAorbs :: Int -> IO ()
testUserAorbs n = do
  conn <- mockDB "test"

  putStrLn $ "\nGenerating mock data for " ++ show n ++ " users..."
  mockBaseAorbAnswers conn n

  putStrLn "Most commonplace A/B choices:"
  common <- getUserTopXMostCommonplace conn 1 3
  mapM_ printAorb common

  putStrLn "Most controversial A/B choices:"
  controversial <- getUserTopXMostControversial conn 1 3
  mapM_ printAorb controversial

  putStrLn "Choices from most controversial to most commonplace:"
  aorbs <- getUserAorbsFromControversialToCommonPlace conn 1
  mapM_ printAorb aorbs

  putStrLn "\nTesting matches between users 1 and 2:"

  putStrLn "\nMain aorbs match:"
  mainMatch <- getMatchesMainAorbs conn 1 2
  case mainMatch of
    Just (a1, a2) -> do
      putStrLn "User 1's main aorb:"
      printMatchingAorb 1 2 a1
      putStrLn "User 2's main aorb:"
      printMatchingAorb 1 2 a2
    Nothing -> putStrLn "No main aorbs found"

  putStrLn "\nTop 3 most unique agreements:"
  similar <- getMatchesTopXUniqueAgreement conn 1 2 3
  mapM_ (printMatchingAorb 1 2) similar

  putStrLn "\nTop 3 most common disagreements:"
  different <- getMatchesTopXCommonDisagreement conn 1 2 3
  mapM_ (printMatchingAorb 1 2) different

  putStrLn "Test complete."
  SQL.close conn
  where
    printAorb :: AorbWithAnswer -> IO ()
    printAorb awa = do
      let aorb = aorbData awa
          answer =
            case userAnswer awa of
              AorbAnswer 0 -> "A: " ++ T.unpack (aorbA aorb)
              AorbAnswer _ -> "B: " ++ T.unpack (aorbB aorb)
          percentageText =
            case userAnswer awa of
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

    printMatchingAorb :: UserID -> UserID -> MatchingAorbWithAnswer -> IO ()
    printMatchingAorb uid1 uid2 mawa = do
      let aorb = matchingAorbData mawa
          answer1 = case mainUserAnswer mawa of
            AorbAnswer 0 -> "A: " ++ T.unpack (aorbA aorb)
            AorbAnswer _ -> "B: " ++ T.unpack (aorbB aorb)
          answer2 = case otherUserAnswer mawa of
            AorbAnswer 0 -> "A: " ++ T.unpack (aorbA aorb)
            AorbAnswer _ -> "B: " ++ T.unpack (aorbB aorb)
          percentB = aorbMean aorb * 100
          percentA = 100 - percentB

      putStrLn $ "  ID: " ++ show (aorbId aorb)
      putStrLn $ "  Context: " ++ T.unpack (aorbCtx aorb)
      putStrLn $ "  Subtext: " ++ T.unpack (aorbStx aorb)
      putStrLn $ "  A: " ++ T.unpack (aorbA aorb)
      putStrLn $ "  B: " ++ T.unpack (aorbB aorb)
      putStrLn $ "  Mean: " ++ show (aorbMean aorb)
      putStrLn $ "  User " ++ show uid1 ++ " chose: " ++ answer1
      putStrLn $ "  User " ++ show uid2 ++ " chose: " ++ answer2
      putStrLn $
        "  Overall: "
        ++ Text.printf "%.1f%% chose A, %.1f%% chose B" percentA percentB
      putStrLn ""

testAorbGaleShapley :: Int -> IO ()
testAorbGaleShapley n
  | n <= 1 = putStrLn "number of matching entities must be greater than two"
  | otherwise = do
  now <- POSIXTime.getPOSIXTime
  let timestamp = DateTimeFormat.formatTime
                  DateTimeFormat.defaultTimeLocale
                  "%Y%m%d%H%M%S"
                  now
      dbName = "data/test-anorby-" ++ timestamp ++ ".db"
  conn <- initDB dbName False
  mockBaseAorbAnswers conn n
  submissions <- baseAorbsToSubmissions conn
  let userIds = Map.keys submissions
      emptyDeprioritization = [(uid, []) | uid <- userIds]
  testGaleShapleyFromRankings
    (submissionsToRankings submissions emptyDeprioritization)
  SQL.close conn

testRandomSubmissionsGaleShapley :: Int -> Int -> Int -> IO ()
testRandomSubmissionsGaleShapley n len seed
  | n <= 1 = putStrLn "number of matching entities must be greater than two"
  | len <= 0 = putStrLn "vector size must be at least one"
  | otherwise =
  let submissions = generateSubmissions n len seed
      userIds = Map.keys submissions
      emptyDeprioritization = [(uid, []) | uid <- userIds]
  in testGaleShapleyFromRankings $
    submissionsToRankings submissions emptyDeprioritization

testRandomRankingsGaleShapley :: Int -> IO ()
testRandomRankingsGaleShapley n
  | n <= 1 = putStrLn "number of matching entities must be greater than two"
  | otherwise = do
  gen <- Random.getStdGen
  testGaleShapleyFromRankings (randomRankings gen n)

testAorbLocalSearch :: Int -> Int -> Double -> Int -> IO ()
testAorbLocalSearch n maxIterations maxBlockingPercentage batchSize
  | n <= 1 = putStrLn "number of matching entities must be greater than two"
  | maxIterations <= 0 = putStrLn "max iterations must be positive"
  | maxBlockingPercentage <= 0.0 = putStrLn "max blocking % must be positive"
  | batchSize <= 1 = putStrLn "batch size must be at least one"
  | otherwise = do
  now <- POSIXTime.getPOSIXTime
  let timestamp = DateTimeFormat.formatTime
                  DateTimeFormat.defaultTimeLocale
                  "%Y%m%d%H%M%S"
                  (POSIXTime.posixSecondsToUTCTime now)
      dbName = "data/test-anorby-" ++ timestamp ++ ".db"
  conn <- initDB dbName False
  mockBaseAorbAnswers conn n
  submissions <- baseAorbsToSubmissions conn
  let userIds = Map.keys submissions
      emptyDeprioritization = [(uid, []) | uid <- userIds]
  testLocalSearchFromRankings
    (submissionsToRankings submissions emptyDeprioritization)
    maxIterations
    maxBlockingPercentage
    batchSize
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
      userIds = Map.keys submissions
      emptyDeprioritization = [(uid, []) | uid <- userIds]
  in testLocalSearchFromRankings
      (submissionsToRankings submissions emptyDeprioritization)
      maxIterations
      maxBlockingPercentage
      batchSize

testRandomRankingsLocalSearch :: Int -> Int -> Double -> Int -> IO ()
testRandomRankingsLocalSearch
  n maxIterations maxBlockingPercentage batchSize
  | n <= 1 = putStrLn "number of matching entities must be greater than two"
  | maxIterations <= 0 = putStrLn "max iterations must be positive"
  | maxBlockingPercentage <= 0.0 = putStrLn "max blocking % must be positive"
  | batchSize <= 1 = putStrLn "batch size must be at least one"
  | otherwise = do
  gen <- Random.getStdGen
  testLocalSearchFromRankings
    (randomRankings gen n)
    maxIterations maxBlockingPercentage batchSize

-- | Helper Functions

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

generateSubmissions :: Int -> Int -> Int -> Submissions
generateSubmissions n len seed = do
  let submissionsList = map (generateSubmission len) [seed .. (seed + n - 1)]
  Map.fromList $ zip [0 ..] submissionsList

generateSubmission :: Int -> Int -> UserSubmission
generateSubmission len seed =
  let binaryVector = randomBinaryVector seed len
      rng = Random.mkStdGen (seed + 1)
      (weightIndex, _) = Random.randomR (0, len - 1) rng
      schemes = [Mirror, Shuffle, Rival]
      schemeIndex = rem seed 3
      scheme = schemes !! schemeIndex
  in (binaryVector, weightIndex, scheme)

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
