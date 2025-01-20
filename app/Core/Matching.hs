module Core.Matching where

import qualified System.Random as Random

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

import Types

fisherYatesShuffle :: (Random.RandomGen g) => g -> [a] -> ([a], g)
fisherYatesShuffle gen [] = ([], gen)
fisherYatesShuffle gen [x] = ([x], gen)
fisherYatesShuffle gen lst =
  let len = length lst
      (index, newGen) = Random.randomR (0, len - 1) gen
      element = lst !! index
      remaining = take index lst ++ drop (index + 1) lst
      (shuffledRest, finalGen) = fisherYatesShuffle newGen remaining
  in (element : shuffledRest, finalGen)

preference :: Rankings -> UserID -> UserID -> UserID -> Ordering
preference ranking me p1 p2 =
  case (rank p1, rank p2) of
    (Just i1, Just i2) -> compare i2 i1
    (Nothing, Nothing) -> EQ
    (Just _, Nothing) -> GT
    (Nothing, Just _) -> LT
  where
    myRanking = Maybe.fromJust $ Map.lookup me ranking
    rank notMe = List.elemIndex notMe myRanking

type ProposalHistory = Map.Map UserID [UserID]

isStableMarriage :: Rankings -> Rankings -> Marriages -> Bool
isStableMarriage group1Rankings group2Rankings marriages =
  let pairs =
        [ (p1, p2)
        | p1 <- Map.keys group1Rankings
        , p2 <- Map.keys group2Rankings
        , blockingPair p1 p2
        ]
  in null pairs
  where
    blockingPair :: UserID -> UserID -> Bool
    blockingPair p1 p2 =
      let maybeP1Partner = Maybe.fromJust $ Map.lookup p1 marriages
          maybeP2Partner = Maybe.fromJust $ Map.lookup p2 marriages
      in case (maybeP1Partner, maybeP2Partner) of
           (Just p1Partner, Just p2Partner) ->
             (preference group1Rankings p1 p2 p1Partner == GT)
              && (preference group2Rankings p2 p1 p2Partner == GT)
           _ -> False

splitRankings :: Rankings -> (Rankings, Rankings)
splitRankings completeRankings =
  let users = Map.keys completeRankings
      n = length users
      midpoint = n `div` 2
      (group1, group2) = List.splitAt midpoint users
      group1Rankings =
        Map.fromList
          [ ( uid
            , List.filter (`elem` group2) $
              Maybe.fromJust $ Map.lookup uid completeRankings
            )
          | uid <- group1
          ]
      group2Rankings =
        Map.fromList
          [ ( uid
            , List.filter (`elem` group1) $
              Maybe.fromJust $ Map.lookup uid completeRankings
            )
          | uid <- group2
          ]
   in (group1Rankings, group2Rankings)

randomSplitRankings :: (Random.RandomGen g)
                    => g -> Rankings
                    -> (Rankings, Rankings)
randomSplitRankings gen completeRankings =
  let users = Map.keys completeRankings
      (shuffledUsers, _) = fisherYatesShuffle gen users
      n = length users
      midpoint = n `div` 2
      (group1, group2) = List.splitAt midpoint shuffledUsers
      group1Rankings = Map.fromList
        [ (uid, List.filter (`elem` group2) $
                            Maybe.fromJust $ Map.lookup uid completeRankings)
        | uid <- group1]
      group2Rankings = Map.fromList
        [ (uid, List.filter (`elem` group1) $
                            Maybe.fromJust $ Map.lookup uid completeRankings)
        | uid <- group2]
  in (group1Rankings, group2Rankings)

galeShapley :: Rankings -> Rankings -> IO Marriages
galeShapley proposerRankings proposeeRankings = do
  putStrLn "\nStarting Gale-Shapley Algorithm..."
  let initialHistory =
        Map.fromList [(uid, []) | uid <- Map.keys proposerRankings]
      initialMarriages =
        Map.fromList
          [ (uid, Nothing)
          | uid <- Map.keys proposerRankings ++ Map.keys proposeeRankings
          ]
  galeShapleyRound
    proposerRankings
    proposeeRankings
    initialMarriages
    initialHistory
    1

galeShapleyRound :: Rankings
                 -> Rankings -> Marriages -> ProposalHistory -> Int
                 -> IO Marriages
galeShapleyRound proposerRankings proposeeRankings marriages history rnd = do
  putStrLn $ "\nRound " ++ show rnd

  let unmatched =
        [ (uid, prefs)
        | uid <- Map.keys proposerRankings
        , Maybe.fromJust (Map.lookup uid marriages) == Nothing
        , let prefs = Maybe.fromJust $ Map.lookup uid proposerRankings
        , let proposed = Maybe.fromJust $ Map.lookup uid history
        , not $ null $ prefs List.\\ proposed
        ]

  if null unmatched
    then do
      putStrLn "Algorithm complete - no more valid proposals possible!"
      return marriages
    else do
      let (proposer, allPrefs)
            | ((x, y):_) <- unmatched = (x, y)
            | otherwise = (-1, [-1]) -- impossible
          proposed = Maybe.fromJust $ Map.lookup proposer history
          nextProposee
            | (x:_) <- allPrefs List.\\ proposed = x
            | otherwise = -1 -- impossible

      putStrLn $
        "Proposer "
        ++ show proposer
        ++ " proposes to "
        ++ show nextProposee

      let newHistory = Map.insert proposer (nextProposee : proposed) history

      let currentMatch = Maybe.fromJust $ Map.lookup nextProposee marriages
      case currentMatch of
        Nothing -> do
          putStrLn $ "Proposee " ++ show nextProposee ++ " accepts."
          let newMarriages =
                Map.insert proposer (Just nextProposee) $
                  Map.insert nextProposee (Just proposer) marriages
          galeShapleyRound
            proposerRankings
            proposeeRankings
            newMarriages
            newHistory
            (rnd + 1)
        Just currentPartner -> do
          let proposeePrefs =
                Maybe.fromJust $ Map.lookup nextProposee proposeeRankings
              currentPartnerIndex =
                Maybe.fromJust $ List.elemIndex currentPartner proposeePrefs
              newProposerIndex =
                Maybe.fromJust $ List.elemIndex proposer proposeePrefs
              prefersNew = newProposerIndex < currentPartnerIndex

          if prefersNew
            then do
              putStrLn $
                "Proposee "
                  ++ show nextProposee
                  ++ " accepts, rejecting "
                  ++ show currentPartner
              let newMarriages =
                    Map.insert proposer (Just nextProposee) $
                      Map.insert nextProposee (Just proposer) $
                        Map.insert currentPartner Nothing marriages
              galeShapleyRound
                proposerRankings
                proposeeRankings
                newMarriages
                newHistory
                (rnd + 1)
            else do
              putStrLn $
                "Proposee "
                ++ show nextProposee
                ++ " rejects in favor of current match."
              galeShapleyRound
                proposerRankings
                proposeeRankings
                marriages
                newHistory
                (rnd + 1)

randomInitialMarriages :: (Random.RandomGen g)
                       => g -> Rankings -> Rankings
                       -> Marriages
randomInitialMarriages gen group1Rankings group2Rankings =
  let group1 = Map.keys group1Rankings
      group2 = Map.keys group2Rankings
      (shuffledGroup2, _) = fisherYatesShuffle gen group2
      pairs = zip group1 shuffledGroup2
   in Map.fromList $
        [(p1, Just p2) | (p1, p2) <- pairs]
          ++ [(p2, Just p1) | (p1, p2) <- pairs]

countBlockingPairs :: Rankings -> Rankings -> Marriages -> Int
countBlockingPairs group1Rankings group2Rankings marriages =
  length
    [ (p1, p2)
    | p1 <- Map.keys group1Rankings
    , p2 <- Map.keys group2Rankings
    , blockingPair p1 p2
    ]
  where
    blockingPair p1 p2 =
      let p1Partner = Maybe.fromJust $ Map.lookup p1 marriages
          p2Partner = Maybe.fromJust $ Map.lookup p2 marriages
       in case (p1Partner, p2Partner) of
            (Just current1, Just current2) ->
              preference group1Rankings p1 p2 (current1) == GT
                && preference group2Rankings p2 p1 (current2) == GT
            _ -> False

blockingPairsPercentage :: Rankings -> Rankings -> Marriages -> Double
blockingPairsPercentage group1Rankings group2Rankings marriages =
  let totalPossiblePairs =
        length (Map.keys group1Rankings) * length (Map.keys group2Rankings)
      blockingPairsCount =
        countBlockingPairs group1Rankings group2Rankings marriages
   in (fromIntegral blockingPairsCount / fromIntegral totalPossiblePairs) * 100

findRandomBlockingPair :: (Random.RandomGen g)
                       => g -> Rankings -> Rankings -> Marriages
                       -> IO (Maybe (UserID, UserID), g)
findRandomBlockingPair gen group1Rankings group2Rankings marriages = do
  let pairs =
        [ (p1, p2)
        | p1 <- Map.keys group1Rankings
        , p2 <- Map.keys group2Rankings
        , blockingPair p1 p2
        ]
      blockingPair p1 p2 =
        let p1Partner = Maybe.fromJust $ Map.lookup p1 marriages
            p2Partner = Maybe.fromJust $ Map.lookup p2 marriages
         in case (p1Partner, p2Partner) of
              (Just current1, Just current2) ->
                preference group1Rankings p1 p2 current1 == GT
                  && preference group2Rankings p2 p1 current2 == GT
              _ -> False
  if null pairs
    then return (Nothing, gen)
    else do
      let (index, newGen) = Random.randomR (0, length pairs - 1) gen
      return (Just (pairs !! index), newGen)

localSearchRound :: Rankings -> Rankings -> Marriages
                 -> Random.StdGen -> Int -> Int -> Double -> Int
                 -> IO Marriages
localSearchRound group1Rankings group2Rankings marriages
  gen iteration maxIterations maxBlockingPercentage batchSize = do
  let currentBlockingPairs =
        countBlockingPairs group1Rankings group2Rankings marriages
      currentPercentage =
        blockingPairsPercentage group1Rankings group2Rankings marriages

  putStrLn $ "\nIteration " ++ show iteration
  putStrLn $ "Current blocking pairs: " ++ show currentBlockingPairs
  putStrLn $ "Blocking pairs percentage: " ++ show currentPercentage ++ "%"

  if iteration >= maxIterations
    then do
      putStrLn "Maximum iterations reached. Terminating..."
      return marriages
    else
      if currentPercentage <= maxBlockingPercentage
        then do
          putStrLn "Achieved target blocking pairs percentage. Terminating..."
          return marriages
        else do
          blockingPairs <- findMultipleBlockingPairs
            gen group1Rankings group2Rankings marriages batchSize
          case blockingPairs of
            [] -> do
              putStrLn "No blocking pairs found. Solution is stable!"
              return marriages
            pairs -> do
              putStrLn $
                "Found "
                ++ show (length pairs)
                ++ " blocking pairs to resolve"
              let newMarriages = resolveMultipleBlockingPairs marriages pairs
              let (newGen, _) = Random.split gen
              localSearchRound
                group1Rankings
                group2Rankings
                newMarriages
                newGen
                (iteration + 1)
                maxIterations
                maxBlockingPercentage
                batchSize

findMultipleBlockingPairs :: Random.RandomGen g
                         => g -> Rankings -> Rankings -> Marriages -> Int
                         -> IO [(UserID, UserID)]
findMultipleBlockingPairs
  gen group1Rankings group2Rankings marriages batchSize = do
  let allPairs =
        [ (p1, p2)
        | p1 <- Map.keys group1Rankings
        , p2 <- Map.keys group2Rankings
        , blockingPair p1 p2
        ]
        where
          blockingPair p1 p2 =
            let p1Partner = Maybe.fromJust $ Map.lookup p1 marriages
                p2Partner = Maybe.fromJust $ Map.lookup p2 marriages
             in case (p1Partner, p2Partner) of
                  (Just current1, Just current2) ->
                    preference group1Rankings p1 p2 current1 == GT
                      && preference group2Rankings p2 p1 current2 == GT
                  _ -> False
      (shuffledPairs, _) = fisherYatesShuffle gen allPairs
  return $ selectNonConflictingPairs [] shuffledPairs batchSize

selectNonConflictingPairs :: [(UserID, UserID)] -> [(UserID, UserID)] -> Int
                          -> [(UserID, UserID)]
selectNonConflictingPairs selected [] _ = selected
selectNonConflictingPairs selected _ 0 = selected
selectNonConflictingPairs selected (pair:remaining) count =
  if canAdd pair selected
    then selectNonConflictingPairs (pair:selected) remaining (count - 1)
    else selectNonConflictingPairs selected remaining count
  where
    canAdd (p1, p2) existing =
      not $ any
        (\(e1, e2) -> p1 == e1 || p1 == e2 || p2 == e1 || p2 == e2)
        existing

resolveMultipleBlockingPairs :: Marriages -> [(UserID, UserID)] -> Marriages
resolveMultipleBlockingPairs = foldl resolveBlockingPair
  where
    resolveBlockingPair marriages (p1, p2) =
      let p1CurrentPartner =
            Maybe.fromJust $ Maybe.fromJust $ Map.lookup p1 marriages
          p2CurrentPartner =
            Maybe.fromJust $ Maybe.fromJust $ Map.lookup p2 marriages
      in Map.insert p1 (Just p2) $
          Map.insert p2 (Just p1) $
          Map.insert p1CurrentPartner (Just p2CurrentPartner) $
          Map.insert p2CurrentPartner (Just p1CurrentPartner) marriages

localSearch :: Rankings -> Rankings -> Int -> Double -> Int -> IO Marriages
localSearch
  group1Rankings group2Rankings
  maxIterations maxBlockingPercentage batchSize = do
  gen <- Random.getStdGen
  let initialMatching =
        randomInitialMarriages gen group1Rankings group2Rankings

  putStrLn
    "Starting Local Search Algorithm..."
  putStrLn $
    "Initial blocking pairs: "
    ++ show (countBlockingPairs group1Rankings group2Rankings initialMatching)
  putStrLn $
    "Batch size: "
    ++ show batchSize

  localSearchRound
    group1Rankings
    group2Rankings
    initialMatching
    gen
    1
    maxIterations
    maxBlockingPercentage
    batchSize
