module LinearClassifier where

import Data.Matrix

type StandardizedSet = [[Double]]
type Data = Matrix Double
type TrainData = ([[Double]],[Bool])
type UpdateParam = Double
type Weight = Double

toData :: [Double] -> Data
toData ilist = fromList 1 (length ilist) ilist

fromData :: Data -> [Double]
fromData = toList


class LinearClassifier p where
  predict :: p -> [Double] -> Bool
  fit :: Int -> [([Double],Bool)] -> p -> p
  weights :: p -> [Weight]

