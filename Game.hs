module Game where

import Player

data State = Running | GameOver deriving (Eq, Show)
data Game = Game
  { state :: State
  , players :: [Player]
  , currentPlayer :: Player
  , shouldClearTerminal :: Bool
  } deriving (Eq, Show)

type Coordinate = (Int, Int)
type Ship = [Coordinate]

boardSize :: Int
boardSize = 10

shipSizes :: [Int]
shipSizes = [2..5]

initialGame :: [Player] -> Bool -> Game
initialGame players  = Game Running players (head players)