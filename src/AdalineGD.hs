module AdalineGD 
(AdalineGD(..))
where

import LinearClassifier
import Lib
import Data.List
--import Numeric.LinearAlgebra

type ActivationFunc = Double -> Double
data AdalineGD = AdalineGD UpdateParam [Weight] deriving (Show)

instance LinearClassifier AdalineGD where
    predict (AdalineGD _ ws) xs = weightScore ws xs > 0
    fit = fitIter
    weights (AdalineGD _ ws) = ws

fitIter :: Int -> ([[Double]],[Bool]) -> AdalineGD -> AdalineGD
fitIter n tdata a0 = iterate (fitData tdata) a0 !! n


fitData :: ([[Double]],[Bool]) -> AdalineGD -> AdalineGD
fitData tdata (AdalineGD eta ws) = AdalineGD eta ws'
    where
      (xss, ybs) = tdata
      ys = map boolToNum ybs
      output = map (weightScore ws) xss
      errors = zipWith (-) ys output
      xssT = transpose xss
      w_corrections = map ((*) eta . sum) (map (zipWith (*) errors) xssT)
      dws = eta * (sum errors) : w_corrections
      ws' = zipWith (+) dws ws

