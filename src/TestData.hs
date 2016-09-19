module TestData where

import Text.CSV (parseCSVFromFile)
import Lib

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


trainData :: IO ([[Double]],[Bool])
trainData = do
  td <- fileData
  let ys  = map (\i -> irisClass i /= "Iris-setosa") $ take 100 td
      xss = map (\i -> [sepalLength i, petalLength i]) $ take 100 td
  return (xss,ys)

trainData_std = standardizeTData <$> trainData
