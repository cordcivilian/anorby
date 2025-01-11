module Marry where

import qualified System.Random as Random

import qualified Data.Maybe as Maybe
import qualified Data.List as List
import qualified Data.Map as Map

type UserID = Int
type Ranking = [UserID]
type Rankings = Map.Map UserID Ranking
type Marriages = Map.Map UserID (Maybe UserID)

preference :: Rankings -> UserID -> UserID -> UserID -> Ordering
preference ranking me p1 p2 = compare (rank p1) (rank p2)
  where
    myRanking = Maybe.fromJust $ Map.lookup me ranking
    rank notMe = List.elemIndex notMe myRanking

isStableMarriage :: Rankings -> Marriages -> Bool
isStableMarriage rankings marriages =
  let pairs = [(p1, p2) | p1 <- Map.keys rankings, p2 <- Map.keys rankings,
               p1 < p2,
               blockingPair p1 p2]
  in null pairs
  where
    blockingPair :: UserID -> UserID -> Bool
    blockingPair p1 p2 =
      let maybeP1Partner = Maybe.fromJust $ Map.lookup p1 marriages
          maybeP2Partner = Maybe.fromJust $ Map.lookup p2 marriages
      in case (maybeP1Partner, maybeP2Partner) of
           (Just p1Partner, Just p2Partner) ->
             (  preference rankings p1 p2 p1Partner == GT
             && preference rankings p2 p1 p2Partner == GT
             )
           _ -> False

randomRankings :: Random.RandomGen g => g -> Int -> Rankings
randomRankings gen n =
  Map.fromList $ zip [1..n] (manyRankings gen n)
  where
    manyRankings :: Random.RandomGen g => g -> Int -> [Ranking]
    manyRankings g size = 
      let (preferences, _) = foldl (randomRanking size) ([], g) [1..size]
      in reverse preferences
    randomRanking :: Random.RandomGen g
                  => Int -> ([Ranking], g) -> Int
                  -> ([Ranking], g)
    randomRanking size (prefs, g) person =
      let others = filter (/= person) [1..size]
          (shuffled, newGen) = fisherYatesShuffle g others
      in (shuffled : prefs, newGen)

fisherYatesShuffle :: Random.RandomGen g => g -> [a] -> ([a], g)
fisherYatesShuffle gen [] = ([], gen)
fisherYatesShuffle gen [x] = ([x], gen)
fisherYatesShuffle gen lst =
  let len = length lst
      (index, newGen) = Random.randomR (0, len - 1) gen
      element = lst !! index
      remaining = take index lst ++ drop (index + 1) lst
      (shuffledRest, finalGen) = fisherYatesShuffle newGen remaining
  in (element : shuffledRest, finalGen)
