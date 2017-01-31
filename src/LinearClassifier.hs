module LinearClassifier where

import Data.Matrix
import Data.Array.Repa

type StandardizedSet = [[Double]]
type Data = Matrix Double
type Input = Array U DIM1 Double
type TrainData = ([[Double]],[Bool])
type UpdateParam = Double
type Weight = Double


class LinearClassifier p where
  predict :: p -> Input -> Bool
  fit :: Int -> [(Input,Bool)] -> p -> p
  weights :: p -> Array U DIM1 Double

class LinearClassifierList p where
  predictL :: p   -> [Double] -> Bool
  fitL     :: Int -> [([Double],Bool)] -> p -> p
  weightsL  :: p -> [Double]

