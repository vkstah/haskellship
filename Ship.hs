module Ship where

type Coordinates = (Int, Int)
data Ship = Ship
  { name :: String
  , coordinates :: (Coordinates, Coordinates)
  , size :: Int
  } deriving (Eq, Show)