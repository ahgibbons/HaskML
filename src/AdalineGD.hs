module AdalineGD 
(AdalineGD(..))
where

import LinearClassifier
import Lib
import Data.List
--import Numeric.LinearAlgebra

type ActivationFunc = Double -> Double
data AdalineGD = AdalineGD Double [Double] deriving (Show)

instance LinearClassifier AdalineGD where
    predict (AdalineGD _ ws) xs = vdot (tail ws) xs + (head ws) > 0
    fit = fitIter
    weights (AdalineGD _ ws) = ws


fitIter :: Int -> ([[Double]],[Bool]) -> AdalineGD -> AdalineGD
fitIter 0 _ a = a
fitIter n tdata a = fitIter (n-1) tdata (fitData tdata a)

fitData :: ([[Double]],[Bool]) -> AdalineGD -> AdalineGD
fitData tdata (AdalineGD eta ws) = AdalineGD eta ws'
    where
      (xss, ybs) = tdata
      ys = map boolToNum ybs
      output = map (\xs -> vdot (tail ws) xs + (head ws)) xss
      errors = zipWith (-) ys output
      xssT = transpose xss
      w_corrections = map ((*) eta . sum) (map (zipWith (*) errors) xssT)
      dws = eta * (sum errors) : w_corrections
      ws' = zipWith (+) dws ws

