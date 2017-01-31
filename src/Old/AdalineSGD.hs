module AdalineSGD 
( AdalineSGD(..)
, AdalineSGDR(..)
, fitShuffleR
,fitData'
, predictR )

where

import LinearClassifier
import Lib
import Data.List
import System.Random.Shuffle
import System.Random
import Control.Monad.Random
import Control.Monad
import Data.Array.Repa hiding (map,tranpose,zipWith)
import qualified Data.Array.Repa as R

data AdalineSGD g = AdalineSGD g UpdateParam [Weight] deriving (Show)
data AdalineSGDR g = 
    AdalineSGDR g UpdateParam (Array U DIM1 Double) deriving (Show)

adalineGen :: RandomGen g => AdalineSGD g -> g
adalineGen (AdalineSGD g _ _) = g

adalineRGen :: RandomGen g => AdalineSGDR g -> g
adalineRGen (AdalineSGDR g _ _) = g

instance RandomGen g => LinearClassifierL (AdalineSGD g) where
  predict (AdalineSGD _ _ ws) xs = weightScore ws xs > 0
  fit = fitShuffle
  weights (AdalineSGD _ _ ws) = ws

fitIter :: RandomGen g => Int -> ([[Double]],[Bool]) -> AdalineSGD g -> AdalineSGD g
fitIter n tdata a0 = iterate (fitData tdata) a0 !! n

fitIterR :: RandomGen g => Int 
         -> [(Input,Bool)] -> AdalineSGDR g
         -> AdalineSGDR g
fitIterR n tdata a0 = iterate (fitDataR tdata) a0 !! n

fitDataR :: RandomGen g => [(Input,Bool)] -> AdalineSGDR g -> AdalineSGDR g
fitDataR tdata a = 
    foldr fitDatumR a tdata

fitDatumR :: RandomGen g => (Input,Bool) -> AdalineSGDR g -> AdalineSGDR g
fitDatumR i@(xs,yb) a@(AdalineSGDR g eta ws) =
    let y   = boolToNum yb
        y'  = sumAllS $ ws *^ xs
        err = y - y'
        dw  = R.map ((eta*err)*) xs
        ws' = computeS $ ws +^ dw
    in (AdalineSGDR g eta ws')

fitShuffle :: RandomGen g => Int -> [([Double],Bool)] 
           -> AdalineSGD g -> AdalineSGD g
fitShuffle n tdata a0 = 
    let g   = adalineGen a0
        rtd = evalRand (replicateM n (shuffleM tdata)) g
    in foldr fitData' a0 rtd

fitShuffleR :: RandomGen g => Int -> [(Input,Bool)]
            -> AdalineSGDR g -> AdalineSGDR g
fitShuffleR n tdata a0 = 
    let g   = adalineRGen a0
        rtd = evalRand (replicateM n (shuffleM tdata)) g
    in foldr fitDataR a0 rtd

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

predictR :: RandomGen g => AdalineSGDR g
         -> Array U DIM1 Double -> Bool
predictR (AdalineSGDR _ _ ws) i = (sumAllS $ ws *^ i) > 0
