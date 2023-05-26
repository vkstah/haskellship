module Logic where

import Data.Char (digitToInt)

import Board ( Board, Cell(Miss, Ship, Hit) )
import Game ( Game(players, currentPlayer), boardSize )
import Player ()
import Ship ( Ship, Coordinates )

switchPlayer :: Game -> Game
switchPlayer game
  | currentPlayer game == head (players game) = game { currentPlayer = last $ players game }
  | otherwise                                 = game { currentPlayer = head $ players game }

fire :: Board -> Coordinates -> Board
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

stringToCoordinates :: String -> Coordinates
stringToCoordinates ['(', x, ',', y, ')'] = (digitToInt x, digitToInt y)
stringToCoordinates _ = (-1, -1)

isValidCoordinates :: Coordinates -> Bool
isValidCoordinates coords
  | fst coords >= 0
    && snd coords >= 0
    && fst coords <= maxSize
    && snd coords <= maxSize  = True
  | otherwise                 = False
  where maxSize = boardSize - 1

isValidShipCoordinates :: Ship -> Bool
isValidShipCoordinates shipCoordinates
  | not (isValidCoordinates (fst shipCoordinates))
    || not (isValidCoordinates (snd shipCoordinates)) = False
  | otherwise = True

-- TODO: Game implementation
--    1. If either of the players' boards has no more Ship cells, the game is over.
--          -> Update the game state to GameOver
--    2. Otherwise return just the game as it is.
checkGameOver :: Game -> Game
checkGameOver game = undefined