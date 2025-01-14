module Rank where

import qualified System.Random as Random

import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Ord as Ord

import Anorby
import Similarity
import Marry

submissionsToRankings :: Submissions -> Rankings
submissionsToRankings submissions = Map.fromList rankings
  where
    userIds = Map.keys submissions 
    rankings = map (makeRanking submissions userIds) userIds

makeRanking :: Submissions -> [UserID] -> UserID -> (UserID, Ranking)
makeRanking submissions allUsers uid = (uid, randomizedRanking)
  where
    (userVector, weightIndex, scheme) = submissions Map.! uid
    (baseline, similarityFunc) = associate scheme
    userWeights = createWeightVector (length userVector) weightIndex 10.0

    similarityScores =
      map (calculateScore submissions userVector userWeights similarityFunc)
          (filter (/= uid) allUsers)

    sortedScores =
      List.sortBy (Ord.comparing (abs . (baseline -) . snd)) similarityScores

    groupedScores = groupByScore sortedScores
    randomizedRanking = concatMap randomizeTies groupedScores

calculateScore :: Submissions
               -> BinaryVector -> WeightVector -> BinaryVectorSimilarity
               -> UserID -> (UserID, Double)
calculateScore submissions userVector userWeights similarityFunc otherId =
  (otherId, similarityScore)
  where
    (otherVector, _, _) = submissions Map.! otherId
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
