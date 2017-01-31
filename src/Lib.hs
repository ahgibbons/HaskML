module Lib
    ( boolToNum
    , vdot
    , standardize
    , lineParam2D
    , standardizeTData
    , checkErrors
    , toRepa
    , weightScore
    , predictError
    , cost
    , fitCost
    , array1D
    ) where

import LinearClassifier
import Data.List (transpose)
import Data.Array.Repa hiding (map,foldr,transpose,zipWith)

array1D :: [Double] -> Array U DIM1 Double
array1D list = fromListUnboxed (Z:.d) list
  where
     d = length list

vdot :: [Double] -> [Double] -> Double
vdot v1 v2 = sum . zipWith (*) v1 $ v2

boolToNum :: Num a => Bool -> a
boolToNum b = if b then 1 else (-1)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

toRepa :: [[Double]] -> Array U DIM2 Double
toRepa xss = let n = length xss
                 d = length (head xss)
             in fromListUnboxed (Z :. n :. d) (concat xss)

standardize :: [Double] -> [Double]
standardize xs = map (\x -> (x - x_mean)/x_std) xs
    where
      x_mean = list_mean xs
      x_std  = list_std xs 


standardizeTData :: [([Double],Bool)] -> [([Double],Bool)]
standardizeTData idata =
    let (xss,bs) = unzip idata
        xss_T = transpose xss
        xss_ST = map standardize xss_T
        xss_S = transpose xss_ST
    in
        zip xss_S bs

list_mean :: [Double] -> Double
list_mean xs = sum xs / fromIntegral (length xs)

list_std :: [Double] -> Double
list_std xs = sqrt . (/ fromIntegral (length xs))
         . sum . map (\x -> (x - (list_mean xs))**2) $ xs

lineParam2D :: LinearClassifier p => p -> [Double]
lineParam2D lc = let [p,q,r] = toList $ weights lc
                 in [-q/r, -p/r]

checkErrors :: LinearClassifier lc => lc -> [(Input,Bool)] -> Int
checkErrors lc d = length . filter (==False) $ map (\(i,y) -> predict lc i == y) d

weightScore :: [Double] -> [Double] -> Double
weightScore ws xs 
    | length xs + 1 == length ws = vdot (tail ws) xs + (head ws)
    | otherwise                  = error "Data/Weights length mismatch"

predictError :: LinearClassifier a => (Input,Bool) -> a -> Double
predictError (xs,yb) a = 
    let y  = boolToNum yb
        y' = sumAllS $ (weights a) *^ xs
    in (y-y')

cost :: LinearClassifier a => (Input,Bool) -> a -> Double
cost i a = let err = predictError i a in 0.5 * (err ^ 2)

fitCost :: LinearClassifier a => [(Input,Bool)] -> a -> Double
fitCost i a = 
    let costs = map (flip cost a) i
    in sum costs / (fromIntegral . length $ costs)
