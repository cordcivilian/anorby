module Core.Ranking where

import qualified System.Random as Random

import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Ord as Ord

import Types
import Core.Similarity
import Core.Matching

makeRanking :: Submissions -> [UserID] -> [UserID] -> UserID
            -> (UserID, Ranking)
makeRanking submissions allUsers deprioritizedIds uid =
  let (userVector, weightIndex, scheme) = submissions Map.! uid
      (baseline, similarityFunc) = associate scheme
      userWeights = createWeightVector (length userVector) weightIndex 10
      similarityScores =
        map (calculateScore submissions userVector userWeights similarityFunc)
            (filter (/= uid) allUsers)
      sortedScores =
        List.sortBy (Ord.comparing (abs . (baseline -) . snd)) similarityScores
      groupedScores = groupByScore sortedScores
      baseRanking = concatMap randomizeTies groupedScores
      (normal, deprioritized) =
        List.partition (\uid' -> uid' `notElem` deprioritizedIds) baseRanking
  in (uid, normal ++ deprioritized)

submissionsToRankings :: Submissions -> [(UserID, [UserID])] -> Rankings
submissionsToRankings submissions deprioritizationMap =
  let userIds = Map.keys submissions
  in Map.fromList
    [ makeRanking submissions userIds deprioritized uid
    | (uid, deprioritized) <- deprioritizationMap ]

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
