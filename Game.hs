module Game where

import Player ( Player )

data State = Running | GameOver deriving (Eq, Show)
data Game = Game
  { state :: State
  , players :: (Player, Player)
  , currentPlayer :: Player
  , shouldClearTerminal :: Bool
  } deriving (Eq, Show)

boardSize :: Int
boardSize = 10

shipTypes :: [(String, Int)]
shipTypes = [("Destroyer", 2), ("Submarine", 3), ("Cruiser", 3), ("Battleship", 4), ("Carrier", 5)]

opponentPlayer :: Game -> Player
opponentPlayer game
  | player == fst (players game)  = snd (players game)
  | otherwise                     = fst (players game)
  where player = currentPlayer game

initialGame :: (Player, Player) -> Bool -> Game
initialGame players = Game Running players (fst players)