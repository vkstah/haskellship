module Game where

import Player

data State = Running | Over (Maybe Player)
data Cell = Empty | Ship | Hit | Miss
data Game = Game
  { state :: State
  , players :: [Player]
  }

start :: [Player] -> IO ()
start ps = undefined