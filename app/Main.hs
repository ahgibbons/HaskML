module Main where

import Lib
import Perceptron
import AdalineGD
import Text.CSV (parseCSVFromFile)
import LinearClassifier
import TestData
import Data.Array.Repa hiding (map, foldr, traverse, zipWith)
import qualified Data.Array.Repa as R
import Data.Matrix as M

main :: IO ()
main = putStrLn "Hi"


andW  = M.fromList 3 1 [-5,4,3]  :: M.Matrix Double
orW   = M.fromList 3 1 [-1,2,3]  :: M.Matrix Double
nandW = M.fromList 3 1 [5,-4,-3] :: M.Matrix Double

ornandW = M.fromLists [[1,-1,5],[0,2,-4],[0,3,-3]] :: M.Matrix Double

boolInputs = [M.fromList 1 3 [1,x,y] | x<-[1,0], y<-[1,0]] :: [M.Matrix Double]

ornandout = map (*ornandW) boolInputs

correctRes = [False,True,True,False]

{-}
verifyW :: (Bool -> Bool -> Bool) -> M.Matrix -> Bool
verifyW bo m = 
  where
    opres = [bo x y | x <- [True,False], y <- [True,False]]
    mres  = [M.fromList 1 3 [1,x,y] | x <- [1,0], y<-[1,0]]
    
-}

weightToBool :: M.Matrix Double -> Bool
weightToBool bmatrix = if (M.getElem 1 1 bmatrix)>0 then True else False
