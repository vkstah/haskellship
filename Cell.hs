module Cell where

data Cell = Empty | Ship | Hit
  deriving (Eq, Show)

isShip :: Cell -> Bool
isShip Ship = True
isShip _ = False

isHit :: Cell -> Bool
isHit Hit = True
isHit _ = False