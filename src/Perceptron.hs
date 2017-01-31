module Perceptron 
( Perceptron(..)
, fitIter
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

--data Perceptron = Perceptron UpdateParam [Weight] deriving (Show)
data Perceptron = Perceptron UpdateParam (Array U DIM1 Double) deriving (Show)

instance LinearClassifier Perceptron where
    predict (Perceptron _ ws) xs = (sumAllS $ ws *^ xs) > 0
    fit = fitIter
    weights (Perceptron _ ws) = ws
 

dotR :: Array U DIM1 Double -> Array U DIM1 Double -> Double
dotR x y = sumAllS (x *^ y)


listToMatrix :: [Double] -> Matrix Double
listToMatrix list = fromList 1 (length list) list


fitDatum :: (Input,Bool) -> Perceptron -> Perceptron
fitDatum (i, yb) p@(Perceptron eta ws) = 
    let y   = boolToNum yb
        y'  = boolToNum $ predict p i
        dw  = R.map ((eta*(y-y'))*) i
        ws' = computeS $ ws +^ dw :: Array U DIM1 Double
    in (Perceptron eta ws')

{-}    
fitDatum :: ([Double],Bool) -> Perceptron -> Perceptron
fitDatum (xs,yb) p@(Perceptron eta ws) =
    let y       = boolToNum yb
        y'      = boolToNum . predictL p $ xs
        diff    = eta*(y-y')
        updates = diff : map (*diff) xs
        ws'     = zipWith (+) ws updates
    in (Perceptron eta ws')
-}

{-}
fitData :: [([Double],Bool)] -> Perceptron -> Perceptron
fitData tdata p = 
    foldr fitDatum p tdata  
-}

fitData :: [(Input,Bool)] -> Perceptron -> Perceptron
fitData tdata p = 
    foldr fitDatum p tdata

{-}
fitIter :: Int -> [([Double],Bool)] -> Perceptron -> Perceptron
fitIter n tdata p0 = iterate (fitData tdata) p0 !! n
-}

fitIter :: Int -> [(Input,Bool)] -> Perceptron -> Perceptron
fitIter n tdata p0 = iterate (fitData tdata) p0 !! n
