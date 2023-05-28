module Player where

import Board ( Board )
import Ship ( Ship )

data Player = Player
  { name :: String
  , ships :: [Ship]
  , board :: Board
  } deriving (Eq, Show)