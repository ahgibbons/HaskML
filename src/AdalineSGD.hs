module AdalineSGD 
( AdalineSGD(..)
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
data AdalineSGD g = AdalineSGD g UpdateParam [Weight] deriving (Show)

adalineGen :: RandomGen g => AdalineSGD g -> g
adalineGen (AdalineSGD g _ _) = g

instance RandomGen g => LinearClassifier (AdalineSGD g) where
  predict (AdalineSGD _ _ ws) xs = weightScore ws xs > 0
  fit = fitShuffle
  weights (AdalineSGD _ _ ws) = ws

fitIter :: RandomGen g => Int -> ([[Double]],[Bool]) -> AdalineSGD g -> AdalineSGD g
fitIter n tdata a0 = iterate (fitData tdata) a0 !! n


fitShuffle :: RandomGen g => Int -> [([Double],Bool)] 
           -> AdalineSGD g -> AdalineSGD g
fitShuffle n tdata a0 = 
    let g   = adalineGen a0
        rtd = evalRand (replicateM n (shuffleM tdata)) g
    in foldr fitData' a0 rtd

fitData' :: RandomGen g => [([Double],Bool)] -> AdalineSGD g -> AdalineSGD g
fitData' tdata a = 
    foldr fitDatum a tdata

fitData :: RandomGen g => ([[Double]],[Bool]) -> AdalineSGD g -> AdalineSGD g
fitData tdata a = 
    foldr fitDatum a (zip (fst tdata) (snd tdata))


fitDatum :: RandomGen g => ([Double],Bool) -> AdalineSGD g -> AdalineSGD g
fitDatum i@(xs,yb) a@(AdalineSGD g eta ws) =
    let err     = eta * predictError i a 
        updates = err : map (*err) xs
        ws'     = zipWith (+) ws updates
    in (AdalineSGD g eta ws')

