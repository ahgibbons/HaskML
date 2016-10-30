module AdalineSGD (AdalineSGD) where

import LinearClassifier
import Lib
import Data.List
--import Numeric.LinearAlgebra

data AdalineSGD = AdalineSGD Double [Double] deriving (Show)

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
