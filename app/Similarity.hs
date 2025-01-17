module Similarity where

import qualified Data.Word as Word
import qualified Data.List as List

import Anorby

associate :: AssociationScheme -> (Double , BinaryVectorSimilarity)
associate scheme =
  case scheme of
    PPPod -> (1.0, weightedSokalSneath)       -- [ 0, 1]
    Balance -> (0.0, weightedYuleQ)           -- [-1, 1]
    Bipolar -> (0.0, weightedRogersTanimoto)  -- [ 0, 1]

createWeightVector :: Int -> Int -> Double -> WeightVector
createWeightVector len index factor =
  [ if i == index then 1.0 * factor else 1.0 | i <- [0..len-1] ]

-- a = number of positions where both vectors have 1
-- b = number of positions where first has 1, second has 0
-- c = number of positions where first has 0, second has 1
-- d = number of positions where both vectors have 0
weightedContingencyTable :: BinaryVector -> BinaryVector -> WeightVector
                         -> WeightedContingency
weightedContingencyTable xs ys weights =
  List.foldl' count (0,0,0,0) $ zip3 xs ys weights
  where
    count (a,b,c,d) (AorbAnswer x, AorbAnswer y, w)
      | x == 1 && y == 1 = (a+w,b,c,d)
      | x == 1 && y == 0 = (a,b+w,c,d)
      | x == 0 && y == 1 = (a,b,c+w,d)
      | otherwise        = (a,b,c,d+w)

validateInputs :: BinaryVector -> BinaryVector -> Either String ()
validateInputs xs ys
  | length xs /= length ys =
    Left "binary vectors must have equal length"
  | not (all (isBinary . unwrapAorbAnswer) xs) =
    Left "first vector must contain only 0 and 1"
  | not (all (isBinary . unwrapAorbAnswer) ys) =
    Left "second vector must contain only 0 and 1"
  | otherwise = Right ()
  where
    unwrapAorbAnswer :: AorbAnswer -> Word.Word8
    unwrapAorbAnswer (AorbAnswer w) = w
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

-- sokal-sneath: more weight on matches [0,1]
weightedSokalSneath :: BinaryVectorSimilarity
weightedSokalSneath xs ys weights =
  let (a,b,c,d) = weightedContingencyTable xs ys weights
  in (2 * (a + d)) / (2*(a + d) + b + c)

-- rogers-tanimoto: more weight on differences [0,1]
weightedRogersTanimoto :: BinaryVectorSimilarity
weightedRogersTanimoto xs ys weights =
  let (a,b,c,d) = weightedContingencyTable xs ys weights
  in (a + d) / (a + d + 2*(b + c))

-- yule's q: good for detecting associations [-1,1]
weightedYuleQ :: BinaryVectorSimilarity
weightedYuleQ xs ys weights =
  let (a,b,c,d) = weightedContingencyTable xs ys weights
      ad = a * d
      bc = b * c
  in if ad + bc == 0 then 0.0 else (ad - bc) / (ad + bc)
