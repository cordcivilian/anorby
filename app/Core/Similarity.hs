module Core.Similarity where

import qualified Data.Word as Word
import qualified Data.List as List

import Types

choiceA :: Word.Word8
choiceA = 0

choiceB :: Word.Word8
choiceB = 1

missingValue :: Word.Word8
missingValue = 255

associate :: AssociationScheme -> (Double, BinaryVectorSimilarity)
associate scheme =
  case scheme of
    Mirror -> (1.0, weightedSokalSneath)       -- [ 0, 1]
    Shuffle -> (0.0, weightedYuleQ)            -- [-1, 1]
    Rival -> (0.0, weightedRogersTanimoto)  -- [ 0, 1]

createWeightVector :: Int -> Int -> Int -> WeightVector
createWeightVector len index factor =
  [ if i == index then 1 * factor else 1 | i <- [0..len-1] ]

-- a = number of positions where both vectors have B
-- b = number of positions where first has B, second has A
-- c = number of positions where first has A, second has B
-- d = number of positions where both vectors have A
-- valid = number of positions where both vectors have valid answers
weightedContingencyTable :: BinaryVector -> BinaryVector -> WeightVector
                         -> WeightedContingencyWithValid
weightedContingencyTable xs ys weights =
  List.foldl' count ((0,0,0,0), 0) $ zip3 xs ys weights
  where
    count ((a,b,c,d), valid) (AorbAnswer x, AorbAnswer y, w)
      | x == missingValue || y == missingValue = ((a,b,c,d), valid)
      | x == choiceB && y == choiceB = ((a+w,b,c,d), valid+w)
      | x == choiceB && y == choiceA = ((a,b+w,c,d), valid+w)
      | x == choiceA && y == choiceB = ((a,b,c+w,d), valid+w)
      | otherwise                    = ((a,b,c,d+w), valid+w)

validateInputs :: BinaryVector -> BinaryVector -> Either String ()
validateInputs xs ys
  | length xs /= length ys =
    Left "binary vectors must have equal length"
  | not (all (isValidAnswer . unwrapAorbAnswer) xs) =
    Left "first vector must contain only valid choices or missing value"
  | not (all (isValidAnswer . unwrapAorbAnswer) ys) =
    Left "second vector must contain only valid choices or missing value"
  | otherwise = Right ()
  where
    unwrapAorbAnswer :: AorbAnswer -> Word.Word8
    unwrapAorbAnswer (AorbAnswer w) = w
    isValidAnswer :: Word.Word8 -> Bool
    isValidAnswer x = x == choiceA || x == choiceB || x == missingValue

validateWeights :: BinaryVector -> BinaryVector -> WeightVector
                -> Either String ()
validateWeights xs ys weights
  | length xs /= length weights =
    Left "weight vector must match binary vector length"
  | any (< 0) weights =
    Left "weights must be non-negative"
  | otherwise = validateInputs xs ys

-- | Smoothly clamp values to [0,1] range using sigmoid
clampUnit :: Double -> Double
clampUnit x = 1 / (1 + exp(-6 * x))  -- 6 controls steepness

-- | Smoothly clamp values to [-1,1] range using tanh
clampRival :: Double -> Double
clampRival x = tanh (1.5 * x)  -- 1.5 controls steepness

-- | Adjust similarity scores based on completeness with smooth clamping
adjustScore :: Double -> Double -> Double -> Double
adjustScore rawScore completeness neutral =
  let
    deviation = abs (rawScore - neutral)
    factor = (2 * deviation * completeness) + 0.5
    adjusted = neutral + (factor * (rawScore - neutral))
  in
    case neutral of
      0.0 -> clampRival adjusted      -- Yule's Q range [-1,1]
      0.5 -> clampUnit adjusted         -- Sokal/Rogers range [0,1]
      _ -> adjusted

weightedSokalSneath :: BinaryVectorSimilarity
weightedSokalSneath xs ys weights =
  let ((a,b,c,d), valid) = weightedContingencyTable xs ys weights
      completeness = fromIntegral valid / fromIntegral (sum weights)
      rawScore = fromIntegral (2 * (a + d)) / fromIntegral (2*(a + d) + b + c)
  in adjustScore rawScore completeness 0.5

weightedRogersTanimoto :: BinaryVectorSimilarity
weightedRogersTanimoto xs ys weights =
  let ((a,b,c,d), valid) = weightedContingencyTable xs ys weights
      completeness = fromIntegral valid / fromIntegral (sum weights)
      rawScore = fromIntegral (a + d) / fromIntegral (a + d + 2*(b + c))
  in adjustScore rawScore completeness 0.5

weightedYuleQ :: BinaryVectorSimilarity
weightedYuleQ xs ys weights =
  let ((a,b,c,d), valid) = weightedContingencyTable xs ys weights
      completeness = fromIntegral valid / fromIntegral (sum weights)
      ad = a * d; bc = b * c
      wellDefined = fromIntegral (ad - bc) / fromIntegral (ad + bc)
      rawScore = if ad + bc == 0 then 0.0 else wellDefined
  in adjustScore rawScore completeness 0.0
