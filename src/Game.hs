module Game where

import Data.Ord

data GType = Fire | Water | Grass | Normal deriving (Show, Eq)

data Effect = NE | NVE | E | SE deriving (Eq,Enum,Ord,Show)

versus :: GType -> GType -> Effect
versus Fire Grass  = SE
versus Fire Water  = NVE

versus Water Fire  = SE
versus Water Grass = NVE

versus Grass Fire  = NVE
versus Grass Water = SE

versus _ _         = E


fight :: GType -> GType -> Maybe GType
fight t1 t2 = case compare (versus t1 t2) (versus t2 t1) of
                LT -> Just t2
                GT -> Just t1
                EQ -> Nothing

damageMult :: Effect -> Double
damageMult NE  = 0
damageMult NVE = 0.5
damageMult E   = 1
damageMult SE  = 2
