module Compare where

import qualified System.Random as Random

import qualified Data.Word as Word
import qualified Data.List as List

type SimilarityScore = Double
type BinaryVector = [Word.Word8]
type Contingency = (Int, Int, Int, Int)
type WeightVector = [Double]
type WeightedContingency = (Double, Double, Double, Double)

data AssociationScheme = Peas
                       | Balance
                       | Polar deriving (Eq, Show)

data SimilarityScheme = Simple
                      | SokalSneath
                      | RogersTanimoto
                      | YuleQ deriving (Eq, Show)

randomBinaryVector :: Int -> Int -> BinaryVector
randomBinaryVector seed len = take len $ randomRs (0, 1) $ Random.mkStdGen seed

randomRs :: (Random.Random a, Random.RandomGen g) => (a, a) -> g -> [a]
randomRs range = unfoldr (Just . Random.randomR range)
  where
    unfoldr f b =
      case f b of
        Just (a, b') -> a : unfoldr f b'
        Nothing      -> []

createWeightVector :: Int -> Int -> Double -> Either String WeightVector
createWeightVector len index factor
  | index >= len = Left "index must be less than vector length"
  | index < 0 = Left "index must be non-negative"
  | factor <= 0 = Left "factor must be positive"
  | otherwise = Right
    [ if i == index then 1.0 * factor else 1.0 | i <- [0..len-1] ]

-- a = number of positions where both vectors have 1
-- b = number of positions where first has 1, second has 0
-- c = number of positions where first has 0, second has 1
-- d = number of positions where both vectors have 0
contingencyTable :: BinaryVector -> BinaryVector -> Contingency
contingencyTable xs ys = List.foldl' count (0,0,0,0) $ zip xs ys
  where
    count (a,b,c,d) (x,y)
      | x == 1 && y == 1 = (a+1,b,c,d)
      | x == 1 && y == 0 = (a,b+1,c,d)
      | x == 0 && y == 1 = (a,b,c+1,d)
      | otherwise        = (a,b,c,d+1)

weightedContingencyTable :: BinaryVector -> BinaryVector -> WeightVector
                         -> WeightedContingency
weightedContingencyTable xs ys weights =
  List.foldl' count (0,0,0,0) $ zip3 xs ys weights
  where
    count (a,b,c,d) (x,y,w)
      | x == 1 && y == 1 = (a+w,b,c,d)
      | x == 1 && y == 0 = (a,b+w,c,d)
      | x == 0 && y == 1 = (a,b,c+w,d)
      | otherwise        = (a,b,c,d+w)

validateInputs :: BinaryVector -> BinaryVector -> Either String ()
validateInputs xs ys
  | length xs /= length ys = Left "binary vectors must have equal length"
  | not (all isBinary xs) = Left "first vector must contain only 0 and 1"
  | not (all isBinary ys) = Left "second vector must contain only 0 and 1"
  | otherwise = Right ()
  where
    isBinary :: Word.Word8 -> Bool
    isBinary x = x <= 1

validateWeights :: BinaryVector -> BinaryVector -> WeightVector
                -> Either String ()
validateWeights xs ys weights
  | length xs /= length weights =
    Left "weight vector must match binary vector length"
  | any (< 0) weights =
    Left "weights must be non-negative"
  | otherwise = validateInputs xs ys

-- simple matching: considers both shared 1s and 0s as matches [0,1]
simple :: BinaryVector -> BinaryVector -> SimilarityScore
simple xs ys =
  let (a,b,c,d) = contingencyTable xs ys
  in fromIntegral (a + d) / fromIntegral (a + b + c + d)

-- sokal-sneath: a more balanced version of simple matching [0,1]
sokalSneath :: BinaryVector -> BinaryVector -> SimilarityScore
sokalSneath xs ys =
  let (a,b,c,d) = contingencyTable xs ys
  in (2 * fromIntegral (a + d)) / fromIntegral (2*(a + d) + b + c)

-- rogers-tanimoto: gives extra weight to differences [0,1]
rogersTanimoto :: BinaryVector -> BinaryVector -> SimilarityScore
rogersTanimoto xs ys =
  let (a,b,c,d) = contingencyTable xs ys
  in fromIntegral (a + d) / fromIntegral (a + d + 2*(b + c))

-- yule's q: good for detecting associations [-1,1]
yuleQ :: BinaryVector -> BinaryVector -> SimilarityScore
yuleQ xs ys =
  let (a,b,c,d) = contingencyTable xs ys
      ad = fromIntegral (a * d)
      bc = fromIntegral (b * c)
  in if ad + bc == 0 then 0.0 else (ad - bc) / (ad + bc)

weightedSimple :: BinaryVector -> BinaryVector -> WeightVector
               -> SimilarityScore
weightedSimple xs ys weights =
  let (a,b,c,d) = weightedContingencyTable xs ys weights
  in (a + d) / (a + b + c + d)

weightedSokalSneath :: BinaryVector -> BinaryVector -> WeightVector
                    -> SimilarityScore
weightedSokalSneath xs ys weights =
  let (a,b,c,d) = weightedContingencyTable xs ys weights
  in (2 * (a + d)) / (2*(a + d) + b + c)

weightedRogersTanimoto :: BinaryVector -> BinaryVector -> WeightVector
                       -> SimilarityScore
weightedRogersTanimoto xs ys weights =
  let (a,b,c,d) = weightedContingencyTable xs ys weights
  in (a + d) / (a + d + 2*(b + c))

weightedYuleQ :: BinaryVector -> BinaryVector -> WeightVector
              -> SimilarityScore
weightedYuleQ xs ys weights =
  let (a,b,c,d) = weightedContingencyTable xs ys weights
      ad = a * d
      bc = b * c
  in if ad + bc == 0 then 0.0 else (ad - bc) / (ad + bc)
