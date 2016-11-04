module AdalineSGD 
( AdalineSGD(..)
,fitPartial
,fitData' )

where

import LinearClassifier
import Lib
import Data.List
import System.Random.Shuffle
import System.Random
import Control.Monad.Random
import Control.Monad
--import Numeric.LinearAlgebra

--   AdalineSGD = AdalineSGD eta weights
data AdalineSGD = RandomGen g => 
                  AdalineSGD g UpdateParam [Weight] deriving (Show)


instance LinearClassifier AdalineSGD where
  predict (AdalineSGD _ _ ws) xs = weightScore ws xs > 0
  fit = fitShuffle (mkStdGen 21)
  weights (AdalineSGD _ _ ws) = ws

fitIter :: Int -> ([[Double]],[Bool]) -> AdalineSGD -> AdalineSGD
fitIter n tdata a0 = iterate (fitData tdata) a0 !! n


fitShuffle :: Int -> [([Double],Bool)] 
           -> AdalineSGD -> AdalineSGD
fitShuffle g n tdata a0@(AdalineSGD g _ _) = 
    let rtd = evalRand (replicateM n (shuffleM tdata)) g
    in foldr fitData' a0 rtd

fitData' :: [([Double],Bool)] -> AdalineSGD -> AdalineSGD
fitData' tdata a = 
    foldr fitDatum a tdata

fitData :: ([[Double]],[Bool]) -> AdalineSGD -> AdalineSGD
fitData tdata a = 
    foldr fitDatum a (zip (fst tdata) (snd tdata))

fitPartial = fitData

fitDatum :: ([Double],Bool) -> AdalineSGD -> AdalineSGD
fitDatum i@(xs,yb) a@(AdalineSGD eta ws) =
    let err     = eta * predictError i a 
        updates = err : map (*err) xs
        ws'     = zipWith (+) ws updates
    in (AdalineSGD eta ws')

