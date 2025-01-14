module Simulate where

import qualified System.Random as Random

import qualified Data.Map as Map

import Anorby
import Similarity
import Rank
import Marry

randomBinaryVector :: Int -> Int -> BinaryVector
randomBinaryVector seed len = take len $ randomRs (0, 1) $ Random.mkStdGen seed

randomRs :: (Random.Random a, Random.RandomGen g) => (a, a) -> g -> [a]
randomRs range = unfoldr (Just . Random.randomR range)
  where
    unfoldr f b =
      case f b of
        Just (a, b') -> a : unfoldr f b'
        Nothing      -> []

createWeightVector :: Int -> Int -> Double -> WeightVector
createWeightVector len index factor =
  [ if i == index then 1.0 * factor else 1.0 | i <- [0..len-1] ]

-- ---------------------------------------------------------------------------

generateCandidates :: Int -> Int -> Int -> Candidates
generateCandidates n len seed = do
  let candidatesList = map (generateCandidate len) [seed .. (seed + n - 1)]
  Map.fromList $ zip [0 ..] candidatesList

generateCandidate :: Int -> Int -> UserSubmission
generateCandidate len seed =
  let binaryVector = randomBinaryVector seed len
      rng = Random.mkStdGen (seed + 1)
      (weightIndex, rng2) = Random.randomR (0, len - 1) rng
      (weightFactor, _) = Random.randomR (1.0, 5.0) rng2
      schemes = [PPPod, Balance, Bipolar]
      schemeIndex = mod seed 3
      scheme = schemes !! schemeIndex
      weightVector = createWeightVector len weightIndex weightFactor
  in (binaryVector, weightVector, scheme)

testCandidatesToRankings :: Int -> Int -> Int -> Rankings
testCandidatesToRankings n len seed =
  candidatesToRankings $ generateCandidates n len seed

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

testRandomCandidatesGaleShapley :: Int -> Int -> Int -> IO ()
testRandomCandidatesGaleShapley n len seed
  | n <= 1 = putStrLn "number of matching entities must be greater than two"
  | len <= 0 = putStrLn "vector size must be at least one"
  | otherwise =
  let candidates = generateCandidates n len seed
  in testGaleShapleyFromRankings $ candidatesToRankings candidates

testRandomRankingsGaleShapley :: Int -> IO ()
testRandomRankingsGaleShapley n
  | n <= 1 = putStrLn "number of matching entities must be greater than two"
  | otherwise = do
  gen <- Random.getStdGen
  let completeRankings = randomRankings gen n
  testGaleShapleyFromRankings completeRankings

testRandomCandidatesLocalSearch :: Int -> Int -> Int -> Int -> Double -> Int
                                -> IO ()
testRandomCandidatesLocalSearch
  n len seed maxIterations maxBlockingPercentage batchSize
  | n <= 1 = putStrLn "number of matching entities must be greater than two"
  | len <= 0 = putStrLn "vector size must be at least one"
  | maxIterations <= 0 = putStrLn "max iterations must be positive"
  | maxBlockingPercentage <= 0.0 = putStrLn "max blocking % must be positive"
  | batchSize <= 1 = putStrLn "batch size must be at least one"
  | otherwise =
  let candidates = generateCandidates n len seed
  in testLocalSearchFromRankings
      (candidatesToRankings candidates)
      maxIterations maxBlockingPercentage batchSize

testRandomRankingsLocalSearch :: Int -> Int -> Double -> Int -> IO ()
testRandomRankingsLocalSearch
  n maxIterations maxBlockingPercentage batchSize = do
  gen <- Random.getStdGen
  let completeRankings = randomRankings gen n
  testLocalSearchFromRankings
    completeRankings maxIterations maxBlockingPercentage batchSize

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
