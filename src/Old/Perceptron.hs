module Perceptron 
( Perceptron(..)
, PerceptronR(..)
, fitIterR
, fitIter
, fitDatumR
, fitDatum)
where

import Data.Either
import Lib
import LinearClassifier
import Data.Matrix hiding (transpose)
import qualified Data.Matrix as M
import Data.Array.Repa hiding (map,zipWith)
import qualified Data.Array.Repa as R
import Data.Array.Repa.Algorithms.Matrix

data Perceptron = Perceptron UpdateParam [Weight] deriving (Show)
data PerceptronR = PerceptronR UpdateParam (Array U DIM1 Double) deriving (Show)

instance LinearClassifierL Perceptron where
    predict (Perceptron _ ws) xs = weightScore ws xs > 0
    fit = fitIter
    weights (Perceptron _ ws) = ws

dotR :: Array U DIM1 Double -> Array U DIM1 Double -> Double
dotR x y = sumAllS (x *^ y)

predictR :: PerceptronR -> Input -> Bool
predictR (PerceptronR _ ws) i = dotR i ws > 0
    

listToMatrix :: [Double] -> Matrix Double
listToMatrix list = fromList 1 (length list) list


fitDatumR :: (Input,Bool) -> PerceptronR -> PerceptronR
fitDatumR (i, yb) p@(PerceptronR eta ws) = 
    let y   = boolToNum yb
        y'  = boolToNum $ predictR p i
        dw  = R.map ((eta*(y-y'))*) i
        ws' = computeS $ ws +^ dw :: Array U DIM1 Double
    in (PerceptronR eta ws')
    
fitDatum :: ([Double],Bool) -> Perceptron -> Perceptron
fitDatum (xs,yb) p@(Perceptron eta ws) =
    let y       = boolToNum yb
        y'      = boolToNum . predictL p $ xs
        diff    = eta*(y-y')
        updates = diff : map (*diff) xs
        ws'     = zipWith (+) ws updates
    in (Perceptron eta ws')


fitData :: [([Double],Bool)] -> Perceptron -> Perceptron
fitData tdata p = 
    foldr fitDatum p tdata  

fitDataR :: [(Input,Bool)] -> PerceptronR -> PerceptronR
fitDataR tdata p = 
    foldr fitDatumR p tdata

fitIter :: Int -> [([Double],Bool)] -> Perceptron -> Perceptron
fitIter n tdata p0 = iterate (fitData tdata) p0 !! n

fitIterR :: Int -> [(Input,Bool)] -> PerceptronR -> PerceptronR
fitIterR n tdata p0 = iterate (fitDataR tdata) p0 !! n
