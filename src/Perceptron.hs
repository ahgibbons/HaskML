module Perceptron 
( Perceptron(..)
, PerceptronM(..)
, fitIter2
, fitDatum2)
where

import Data.Either
import Lib
import LinearClassifier
import Data.Matrix

data Perceptron = Perceptron UpdateParam [Weight] deriving (Show)

data PerceptronM = PerceptronM UpdateParam (Matrix Weight) deriving (Show)

instance LinearClassifier Perceptron where
    predict (Perceptron _ ws) xs = weightScore ws xs > 0
    fit = fitIter
    weights (Perceptron _ ws) = ws


listToMatrix :: [Double] -> Matrix Double
listToMatrix list = fromList 1 (length list) list

fitDatum2 :: ([Double],Bool) -> PerceptronM -> PerceptronM
fitDatum2 (xs,yb) p@(PerceptronM eta ws) =
    let i  = fromList 1 (length xs + 1) (1:xs)
        y  = boolToNum yb
        y' = boolToNum $ (getElem 1 1 $ i * ws) > 0
        dw = scaleMatrix (eta*(y-y')) (transpose i)
        ws' = ws + dw
    in (PerceptronM eta ws')
    
fitDatum :: ([Double],Bool) -> Perceptron -> Perceptron
fitDatum (xs,yb) p@(Perceptron eta ws) =
    let y       = boolToNum yb
        y'      = boolToNum . predict p $ xs
        diff    = eta*(y-y')
        updates = diff : map (*diff) xs
        ws'     = zipWith (+) ws updates
    in (Perceptron eta ws')


fitData :: [([Double],Bool)] -> Perceptron -> Perceptron
fitData tdata p = 
    foldr fitDatum p tdata  

fitData2 :: [([Double],Bool)] -> PerceptronM -> PerceptronM
fitData2 tdata p = 
    foldr fitDatum2 p tdata

fitIter :: Int -> [([Double],Bool)] -> Perceptron -> Perceptron
fitIter n tdata p0 = iterate (fitData tdata) p0 !! n

fitIter2 :: Int -> [([Double],Bool)] -> PerceptronM -> PerceptronM
fitIter2 n tdata p0 = iterate (fitData2 tdata) p0 !! n
