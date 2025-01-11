{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Compare as Compare
import qualified Rank as Rank
import qualified Marry as Marry

-- 5001
main :: IO ()
main = do
  let len = 10
  let v1 = Compare.randomBinaryVector 0 len
  let v2 = Compare.randomBinaryVector 1 len
  let eitherW1 = Compare.createWeightVector len 0 5
  let eitherW2 = Compare.createWeightVector len 9 5
  print v1
  print v2
  print $ Compare.simple v1 v2
  print $ Compare.yuleQ v1 v2
  case (eitherW1, eitherW2) of
    (Right w1, Right w2) -> do
      print $ Compare.weightedSimple v1 v2 w1
      print $ Compare.weightedYuleQ v2 v1 w2
    (_, _)               -> putStrLn "invalid weights"
