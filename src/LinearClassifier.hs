module LinearClassifier where

type StandardizedSet = [[Double]]
type Data = [Double]
type TrainData = ([Data],[Bool])
type UpdateParam = Double
type Weight = Double

class LinearClassifier p where
  predict :: p -> Data -> Bool
  fit :: Int -> [([Double],Bool)] -> p -> p
  weights :: p -> [Weight]

