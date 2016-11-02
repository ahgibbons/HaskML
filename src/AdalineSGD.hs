module AdalineSGD 
(AdalineSGD(..))

where

import LinearClassifier
import Lib
import Data.List
import System.Random.Shuffle
import System.Random
--import Numeric.LinearAlgebra

--   AdalineSGD = AdalineSGD eta weights
data AdalineSGD = AdalineSGD Double [Double] deriving (Show)

{-}
fitIter :: (RandomGen g) => 
           g -> Int -> ([[Double]],[Bool]) 
        -> AdalineSGD -> AdalineSGD
fitIter 0 
fitIter g n tdata (AdalineSGD eta ws) = 
    
fitStep :: (RandomGen g) => g
        -> ([[Double]],[Bool])
        -> AdalineSGD -> AdalineSGD
fitStep g (xs,ys) a@(AdalineSGD eta ws) = 
    let cost = map updateWeight tdata
        avg_cost = sum (cost) / (fromIntegral $ length cost)
    in avg_cost



net_input = map (\xs -> vdot (tail ws) xs + (head ws)) xss

-}
{-}
instance LinearClassifier AdalineSGD where
    predict (AdalineSGD _ ws) xs = vdot (tail ws) xs + (head ws) > 0
    fit = fitIter
    weights (AdalineSGD _ ws) = ws


fitDatum :: ([Double], Bool) -> AdalineSGD -> AdalineSGD
fitDatum (xs,yb) a@(Adaline eta ws) =
    let y  = boolToNum yb
        output = map (\xs -> vdot (tail ws) xs + (head ws)) xss
        error = eta*(y-output)
        updates = diff : map (*diff) xs
        ws' = zipWith (+) ws updates
    in (AdalineSGD eta ws')
-}
