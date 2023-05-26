module Game where

import Player ( Player )

data State = Running | GameOver deriving (Eq, Show)
data Game = Game
  { state :: State
  , players :: [Player]
  , currentPlayer :: Player
  , shouldClearTerminal :: Bool
  } deriving (Eq, Show)

boardSize :: Int
boardSize = 10

shipSizes :: [Int]
shipSizes = [2..5]

shipTypes :: [(String, Int)]
shipTypes = [("Destroyer", 2), ("Submarine", 3), ("Cruiser", 3), ("Battleship", 4), ("Carrier", 5)]

initialGame :: [Player] -> Bool -> Game
initialGame players = Game Running players (head players)