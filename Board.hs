module Board where

data Cell = Empty | Ship | Hit | Miss deriving (Eq, Show)
type Board = [[Cell]]

emptyBoard :: Board
emptyBoard = replicate 10 $ replicate 10 Empty