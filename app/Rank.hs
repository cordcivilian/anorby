module Rank where

import System.Random as Random

import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Ord as Ord

import Similarity
import Marry

type UserSubmission = (BinaryVector, WeightVector, AssociationScheme)
type Candidates = Map.Map UserID UserSubmission

candidatesToRankings :: Candidates -> Rankings
candidatesToRankings candidates = Map.fromList rankings
  where
    userIds = Map.keys candidates
    rankings = map (makeRanking candidates userIds) userIds

makeRanking :: Candidates -> [UserID] -> UserID -> (UserID, Ranking)
makeRanking candidates allUsers userId = (userId, randomizedRanking)
  where
    (userVector, userWeights, scheme) = candidates Map.! userId
    (baseline, similarityFunc) = associate scheme

    similarityScores =
      map (calculateScore candidates userVector userWeights similarityFunc)
          (filter (/= userId) allUsers)

    sortedScores =
      List.sortBy (Ord.comparing (abs . (baseline -) . snd)) similarityScores

    groupedScores = groupByScore sortedScores
    randomizedRanking = concatMap randomizeTies groupedScores

calculateScore :: Candidates
               -> BinaryVector -> WeightVector -> BinaryVectorSimilarity
               -> UserID -> (UserID, Double)
calculateScore candidates userVector userWeights similarityFunc otherId =
  (otherId, similarityScore)
  where
    (otherVector, _, _) = candidates Map.! otherId
    similarityScore = similarityFunc userVector otherVector userWeights

groupByScore :: [(UserID, Double)] -> [[(UserID, Double)]]
groupByScore [] = []
groupByScore (x:xs) = (x : same) : groupByScore different
  where (same, different) = span ((== snd x) . snd) xs

randomizeTies :: [(UserID, Double)] -> [UserID]
randomizeTies group =
  let userIDs = map fst group
      gen = Random.mkStdGen 21
      (shuffledIDs, _) = fisherYatesShuffle gen userIDs
  in shuffledIDs

generateCandidates :: Int -> Int -> Int -> Either String Candidates
generateCandidates n len seed
  | n <= 0 = Left "number of candidates must be positive"
  | len <= 0 = Left "vector length must be positive"
  | otherwise = do
      let candidatesList = map (generateCandidate len) [seed .. (seed + n - 1)]
      candidates <- sequence candidatesList
      return $ Map.fromList $ zip [0 ..] candidates

generateCandidate :: Int -> Int -> Either String UserSubmission
generateCandidate len seed = do
  let binaryVector = randomBinaryVector seed len
  let rng = mkStdGen (seed + 1)
  let (weightIndex, rng2) = randomR (0, len - 1) rng
  let (weightFactor, _) = randomR (1.0, 5.0) rng2
  let schemes = [PPPod, Balance, Bipolar]
  let schemeIndex = mod seed 3
  let scheme = schemes !! schemeIndex
  let eitherWeightVector = createWeightVector len weightIndex weightFactor
  case eitherWeightVector of
    Right weightVector -> Right (binaryVector, weightVector, scheme)
    Left err -> Left err

testCandidatesToRankings :: Int -> Int -> Int -> Rankings
testCandidatesToRankings n len seed =
  case generateCandidates n len seed of
    Right candidates -> candidatesToRankings candidates
    Left _ -> Map.fromList [(1, [2]), (2, [1])]
