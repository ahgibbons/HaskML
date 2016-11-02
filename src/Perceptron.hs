module Perceptron 
( Perceptron(..))
where

import Data.Either
import Lib
import LinearClassifier

data Perceptron = Perceptron Double [Double] deriving (Show)

instance LinearClassifier Perceptron where
    predict (Perceptron _ ws) xs = vdot (tail ws) xs + (head ws) > 0
    fit = fitIter
    weights (Perceptron _ ws) = ws

    
fitDatum :: ([Double],Bool) -> Perceptron -> Perceptron
fitDatum (xs,yb) p@(Perceptron eta ws) =
    let y    = boolToNum yb
        y'   = boolToNum . predict p $ xs
        diff = eta*(y-y')
        updates = diff : map (*diff) xs
        ws' = zipWith (+) ws updates
    in (Perceptron eta ws')

fitData :: ([[Double]],[Bool]) -> Perceptron -> Perceptron
fitData tdata p = 
    foldr fitDatum p (zip (fst tdata) (snd tdata))  


fitIter :: Int -> ([[Double]],[Bool]) -> Perceptron -> Perceptron
fitIter n tdata p0 = iterate (fitData tdata) p0 !! n
