module Logic where

import Board
import Game

switchPlayer :: Game -> Game
switchPlayer game
  | currentPlayer game == head (players game) = game { currentPlayer = last $ players game }
  | otherwise                                 = game { currentPlayer = head $ players game }

fire :: Board -> Coordinate -> Board
fire = undefined

isEmpty :: Cell -> Bool
isEmpty Miss = True
isEmpty _ = False

isShip :: Cell -> Bool
isShip Ship = True
isShip _ = False

isHit :: Cell -> Bool
isHit Hit = True
isHit _ = False

isMiss :: Cell -> Bool
isMiss Miss = True
isMiss _ = False