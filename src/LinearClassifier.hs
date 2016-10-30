module LinearClassifier where

data DataForm = Normal [[Double]] | Standardized [[Double]]

class LinearClassifier p where
  predict :: p -> [Double] -> Bool
  fit :: Int -> ([[Double]],[Bool]) -> p -> p
  weights :: p -> [Double]

