module Rank where

import qualified System.Random as Random

import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Ord as Ord

import Similarity
import Marry
import Anorby

type UserSubmission = (BinaryVector, WeightVector, AssociationScheme)
type Candidates = Map.Map UserID UserSubmission

candidatesToRankings :: Candidates -> Rankings
candidatesToRankings candidates = Map.fromList rankings
  where
    userIds = Map.keys candidates
    rankings = map (makeRanking candidates userIds) userIds

makeRanking :: Candidates -> [UserID] -> UserID -> (UserID, Ranking)
makeRanking candidates allUsers uid = (uid, randomizedRanking)
  where
    (userVector, userWeights, scheme) = candidates Map.! uid
    (baseline, similarityFunc) = associate scheme

    similarityScores =
      map (calculateScore candidates userVector userWeights similarityFunc)
          (filter (/= uid) allUsers)

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
