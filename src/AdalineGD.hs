module AdalineGD 
( AdalineGD(..)
, AdalineGDR(..)
, fitData
, fitIter
, fitDataR
, fitIterR
, predictR)
where

import LinearClassifier
import Lib
import Data.List
import Data.Array.Repa hiding (map,zipWith,transpose)
import qualified Data.Array.Repa as R
import Data.Array.Repa.Algorithms.Matrix
--import Numeric.LinearAlgebra

type ActivationFunc = Double -> Double
data AdalineGD = AdalineGD UpdateParam [Weight] deriving (Show)
data AdalineGDR = AdalineGDR UpdateParam (Array U DIM1 Double) deriving (Show)

instance LinearClassifier AdalineGD where
    predict (AdalineGD _ ws) xs = weightScore ws xs > 0
    fit = fitIter
    weights (AdalineGD _ ws) = ws

fitIter :: Int -> [([Double],Bool)] -> AdalineGD -> AdalineGD
fitIter n tdata a0 = iterate (fitData tdata) a0 !! n

fitIterR :: Int -> [([Double],Bool)] -> AdalineGDR -> AdalineGDR
fitIterR n tdata a0 = iterate (fitDataR (xssR,ybr)) a0 !! n
   where
     (xss,ybs) = unzip tdata
     d = length (head xss) + 1
     n = length xss
     xss' = map (1:) xss
     xssR = fromListUnboxed (Z:.n:.d::DIM2) (concat xss') :: Array U DIM2 Double
     ybr = fromListUnboxed (Z:.n::DIM1) ybs :: Array U DIM1 Bool


fitData :: [([Double],Bool)] -> AdalineGD -> AdalineGD
fitData tdata (AdalineGD eta ws) = AdalineGD eta ws'
    where
      (xss, ybs) = unzip tdata
      ys = map boolToNum ybs
      output = map (weightScore ws) xss
      errors = zipWith (-) ys output
      xssT = transpose xss
      w_corrections = map ((*) eta . sum) (map (zipWith (*) errors) xssT)
      dws = eta * (sum errors) : w_corrections
      ws' = zipWith (+) dws ws

fitDataR :: (Array U DIM2 Double, Array U DIM1 Bool) -> AdalineGDR -> AdalineGDR
fitDataR (xsr,ybr) (AdalineGDR eta ws) = AdalineGDR eta ws'
    where
      (Z:.n:.d) = extent xsr
      ysr  = computeS . reshape (Z:.n:.1) . R.map boolToNum $ ybr :: Array U DIM2 Double
      ysr' = xsr `mmultS` (computeS $ reshape (Z:.d:.1::DIM2) ws)
      error = computeS $ ysr -^ ysr'
      xsrT = computeS $ R.transpose xsr
      dw = R.map (*(eta::Double)) $ xsrT `mmultS` error
      ws' = computeS $ ws +^ reshape (Z:.d::DIM1) dw
      
predictR :: AdalineGDR -> Array U DIM1 Double -> Bool
predictR (AdalineGDR _ ws) i = (sumAllS $ ws *^ i) > 0
