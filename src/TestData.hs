module TestData where

import Text.CSV (parseCSVFromFile)
import Lib
import Data.Array.Repa hiding (map,foldr)
import LinearClassifier
import Perceptron
import AdalineGD
import AdalineSGD
import System.Random

data Iris = Iris { sepalLength :: Double
                 , sepalWidth :: Double
                 , petalLength :: Double
                 , petalWidth :: Double
                 , irisClass :: String}

recToIris :: [String] -> Iris
recToIris [sL,sW,pL,pW,iC] = Iris { sepalLength = read sL
                                  , sepalWidth = read sW
                                  , petalLength = read pL
                                  , petalWidth = read pW
                                  , irisClass = iC}


fileData = do 
    Right t <- parseCSVFromFile "iris.data"
    let t' = map recToIris . filter (/= [""]) $ t
    return t'


trainData :: IO [([Double],Bool)]
trainData = do
  td <- fileData
  let ys  = map (\i -> irisClass i /= "Iris-setosa") $ take 100 td
      xss = map (\i -> [sepalLength i, petalLength i]) $ take 100 td
  return $ zip xss ys

trainData_std = standardizeTData <$> trainData

trainData_repa :: IO [(Input,Bool)]
trainData_repa = 
    map (\(xs,yb) -> (fromListUnboxed (Z:.3::DIM1) (1:xs),yb)) <$> trainData_std

{-}
perceptron_sample = do
  td <- trainData
  return $ fit 20 td (Perceptron 0.01 [0,0,0])

perceptron_std_sample = do
  td <- trainData_std
  return $ fit 20 td (Perceptron 0.01 [0,0,0])

adalineGD_sample = do
  td <- trainData_std
  return $ fit 20 td (AdalineGD 0.01 [0,0,0])

adalineSGD_sample = do
  td <- trainData_std
  g <- newStdGen
  return $ fit 20 td (AdalineSGD g 0.01 [0,0,0])

adalineGDR_sample = do
  td <- trainData_std
  return $ AdalineGD.fitIterR 20 td (AdalineGDR 0.01 (fromListUnboxed (Z:.3) [0,0,0]))

adalineSGDR_sample = do
  td  <- trainData_repa
  g <- newStdGen
  let a0 = AdalineSGDR g 0.01 (fromListUnboxed (Z:.3::DIM1) [0,0,0])
  return $ AdalineSGD.fitShuffleR 20 td a0
-}
