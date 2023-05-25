module Game where

data State = Running | GameOver deriving (Eq, Show)
data Game = Game
  { state :: State
  , players :: [Player]
  , currentPlayer :: Player
  } deriving (Eq, Show)
newtype Player = Player
  { name :: String
  } deriving (Eq, Show)

type Coordinate = (Int, Int)
type Ship = [Coordinate]

boardSize :: Int
boardSize = 10

shipSizes :: [Int]
shipSizes = [2..5]