module Handwriting where

import Control.Monad.State

name="Andrew"

data Link = Link Node Double deriving (Show,Eq)
data Node = Node [Link] Double | Input Double deriving (Show,Eq)
type Layer = [Node]




evalNode :: Node -> Double
evalNode (Input a) = a
evalNode (Node links bias) = bias + (sum . map evalLink $ links)
  where
    evalLink (Link node weight) = evalNode node * weight

evalLayer :: Layer -> [Double]
evalLayer = map evalNode



ilayer = [Input 1, Input 2]
layer1 = let links = [Link i 0 | i <- ilayer]
         in [Node links 1, Node links 2, Node links 3]

olayer = let links = [Link i 0 | i <- layer1]
         in [Node links 0]
