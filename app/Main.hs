module Main where

import Lib
import Perceptron
import AdalineGD
import Text.CSV (parseCSVFromFile)
import LinearClassifier
import TestData
import Data.Array.Repa hiding (map, foldr, traverse, zipWith)
import qualified Data.Array.Repa as R

main :: IO ()
main = putStrLn "Hi"
