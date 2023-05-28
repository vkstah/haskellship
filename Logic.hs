module Logic where

import Data.Char (digitToInt)

import Board ( Board, Cell(Empty, Miss, Hit) )
import Game ( Game(players, currentPlayer), boardSize )
import Ship ( Ship, Coordinates )

switchPlayer :: Game -> Game
switchPlayer game
  | currentPlayer game == fst (players game)  = game { currentPlayer = snd $ players game }
  | otherwise                                 = game { currentPlayer = fst $ players game }

fire :: Board -> Coordinates -> Board
fire = undefined

isEmpty :: Cell -> Bool
isEmpty Miss = True
isEmpty _ = False

isHit :: Cell -> Bool
isHit Hit = True
isHit _ = False

isMiss :: Cell -> Bool
isMiss Miss = True
isMiss _ = False

stringToCoordinates :: String -> Coordinates
stringToCoordinates (x:y)
  | not $ yIsValid y = (-1, -1)
  | x == 'A' = (0, stringToIntMinusOne y)
  | x == 'B' = (1, stringToIntMinusOne y)
  | x == 'C' = (2, stringToIntMinusOne y)
  | x == 'D' = (3, stringToIntMinusOne y)
  | x == 'E' = (4, stringToIntMinusOne y)
  | x == 'F' = (5, stringToIntMinusOne y)
  | x == 'G' = (6, stringToIntMinusOne y)
  | x == 'H' = (7, stringToIntMinusOne y)
  | x == 'I' = (8, stringToIntMinusOne y)
  | x == 'J' = (9, stringToIntMinusOne y)
  | otherwise = (-1, -1)
  where stringToIntMinusOne str = read str - 1
        yIsValid str            = stringToIntMinusOne str >= 0 && stringToIntMinusOne str <= 9

isValidCoordinates :: Coordinates -> Bool
isValidCoordinates coords
  | fst coords >= 0
    && snd coords >= 0
    && fst coords <= maxSize
    && snd coords <= maxSize  = True
  | otherwise                 = False
  where maxSize = boardSize - 1

isValidShipCoordinates :: Ship -> Int -> Bool
isValidShipCoordinates shipCoordinates size
  | not (isValidCoordinates (fst shipCoordinates))
    || not (isValidCoordinates (snd shipCoordinates)) = False
  | not $ isShipHorizontal || isShipVertical = False
  | isShipHorizontal && horizontalDiff /= size - 1 = False
  | isShipVertical && verticalDiff /= size - 1 = False
  | otherwise = True
  where isShipHorizontal  = isRangeHorizontal shipCoordinates
        isShipVertical    = isRangeVertical shipCoordinates
        horizontalDiff    = snd (snd shipCoordinates) - snd (fst shipCoordinates)
        verticalDiff      = fst (snd shipCoordinates) - fst (fst shipCoordinates)

isValidCoordinatesRange :: [String] -> Bool
isValidCoordinatesRange str = undefined

isRangeHorizontal :: (Coordinates, Coordinates) -> Bool
isRangeHorizontal (x,y)
  | fst x == fst y  = True
  | otherwise       = False

isRangeVertical :: (Coordinates, Coordinates) -> Bool
isRangeVertical (x,y)
  | snd x == snd y  = True
  | otherwise       = False

-- TODO: Game implementation
--    1. If either of the players' boards has no more Ship cells, the game is over.
--          -> Update the game state to GameOver
--    2. Otherwise return just the game as it is.
checkGameOver :: Game -> Game
checkGameOver game = undefined