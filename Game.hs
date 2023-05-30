module Game where

import Player ( Player )
import Ship ( Ship )

data State = Running | GameOver deriving (Eq, Show)
data Game = Game
  { state :: State
  , playerOne :: Player
  , playerTwo :: Player
  , currentPlayer :: Player
  , shouldClearTerminal :: Bool
  , debug :: Bool
  } deriving (Eq, Show)

boardSize :: Int
boardSize = 10

shipTypes :: [(String, Int)]
shipTypes = [("Destroyer", 2), ("Submarine", 3), ("Cruiser", 3), ("Battleship", 4), ("Carrier", 5)]

initialShips :: [Ship]
initialShips = []

initialGame :: (Player, Player) -> Bool -> Bool -> Game
initialGame players = uncurry (Game Running) players (fst players)
