module Logic where

import Data.Char

import Board ( Board, Cell(Empty, Miss, Hit), emptyBoard )
import Game ( Game(state, playerOne, playerTwo, currentPlayer), boardSize )
import Player ( Player )
import Ship ( Ship(name, coordinates, size), Coordinates )
import Utility ( transformList )

switchPlayer :: Game -> Game
switchPlayer game
  | currentPlayer game == playerOne game  = game { currentPlayer = playerTwo game }
  | otherwise                             = game { currentPlayer = playerOne game }

isEmptyCell :: Cell -> Bool
isEmptyCell Empty = True
isEmptyCell _ = False

isHitCell :: Cell -> Bool
isHitCell Hit = True
isHitCell _ = False

isMissCell :: Cell -> Bool
isMissCell Miss = True
isMissCell _ = False

mapCellToBoard :: Cell -> Char
mapCellToBoard cell
  | isHitCell cell = 'x'
  | isEmptyCell cell = ' '
  | isMissCell cell = 'o'
  | otherwise = '?'

fireHitShip :: Coordinates -> [Ship] -> Bool
fireHitShip coords ships = undefined

markMiss :: Coordinates -> Board -> Board
markMiss (x,y) = transformList x (transformList y Miss (emptyBoard!!x))

markHit :: Coordinates -> Board -> Board
markHit (x,y) = transformList x (transformList y Hit (emptyBoard!!x))

opponentPlayer :: Game -> Player
opponentPlayer game
  | currentPlayer game == playerOne game  = playerTwo game
  | otherwise                             = playerOne game

stringToCoordinates :: String -> Coordinates
stringToCoordinates [x,y]
  | not $ yIsValid y = (-1, -1)
  | x == 'A' = (0, charToIntMinusOne y)
  | x == 'B' = (1, charToIntMinusOne y)
  | x == 'C' = (2, charToIntMinusOne y)
  | x == 'D' = (3, charToIntMinusOne y)
  | x == 'E' = (4, charToIntMinusOne y)
  | x == 'F' = (5, charToIntMinusOne y)
  | x == 'G' = (6, charToIntMinusOne y)
  | x == 'H' = (7, charToIntMinusOne y)
  | x == 'I' = (8, charToIntMinusOne y)
  | x == 'J' = (9, charToIntMinusOne y)
  | otherwise = (-1, -1)
  where charToIntMinusOne c = digitToInt c - 1 :: Int
        yIsValid c          = isDigit c && charToIntMinusOne c >= 0 && charToIntMinusOne c <= 9
stringToCoordinates _ = (-1, -1)

isValidCoordinates :: Coordinates -> Bool
isValidCoordinates coords
  | fst coords >= 0
    && snd coords >= 0
    && fst coords <= maxSize
    && snd coords <= maxSize  = True
  | otherwise                 = False
  where maxSize = boardSize - 1

isValidShipCoordinates :: (Coordinates, Coordinates) -> Int -> [Ship] -> Bool
isValidShipCoordinates coords size board
  | not (isValidCoordinates (fst coords))
    || not (isValidCoordinates (snd coords))        = False
  | not $ isShipHorizontal || isShipVertical        = False
  | isShipHorizontal && horizontalDiff /= size - 1  = False
  | isShipVertical && verticalDiff /= size - 1      = False
  | otherwise                                       = True
  where isShipHorizontal  = isRangeHorizontal coords
        isShipVertical    = isRangeVertical coords
        horizontalDiff    = snd (snd coords) - snd (fst coords)
        verticalDiff      = fst (snd coords) - fst (fst coords)

isValidCoordinatesRange :: (Coordinates, Coordinates) -> Bool
isValidCoordinatesRange coords
  | not (isValidCoordinates (fst coords))
    || not (isValidCoordinates (snd coords))                    = False
  | not $ isRangeHorizontal coords || isRangeHorizontal coords  = False
  | isRangeHorizontal coords && horizontalDiff <= 0             = False
  | isRangeVertical coords && verticalDiff <= 0                 = False
  | otherwise                                                   = True
  where horizontalDiff    = snd (snd coords) - snd (fst coords)
        verticalDiff      = fst (snd coords) - fst (fst coords)

isRangeHorizontal :: (Coordinates, Coordinates) -> Bool
isRangeHorizontal (x,y)
  | fst x == fst y  = True
  | otherwise       = False

isRangeVertical :: (Coordinates, Coordinates) -> Bool
isRangeVertical (x,y)
  | snd x == snd y  = True
  | otherwise       = False

transformGame :: Game -> Game
transformGame game = undefined

-- TODO: Game implementation
--    1. If either of the players' boards has no more Ship cells, the game is over.
--          -> Update the game state to GameOver
--    2. Otherwise return just the game as it is.
checkGameOver :: Game -> Game
checkGameOver game = undefined