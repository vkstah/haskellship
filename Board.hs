module Board where

data Cell = Empty | Ship | Hit | Miss
type Board = [[Cell]]