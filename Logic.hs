module Logic where

import Data.Char (digitToInt)

import Board ( Board, Cell(Empty, Miss, Hit), emptyBoard )
import Game ( Game(state, players, currentPlayer), boardSize )
import Ship ( Ship(name, coordinates, size), Coordinates )

transformList :: (Eq a, Num a, Enum a) => a -> b -> [b] -> [b]
transformList x y = zipWith (\ i v -> (if i == x then y else v)) [0 .. ]

switchPlayer :: Game -> Game
switchPlayer game
  | currentPlayer game == fst (players game)  = game { currentPlayer = snd $ players game }
  | otherwise                                 = game { currentPlayer = fst $ players game }

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

isValidShipCoordinates :: (Coordinates, Coordinates) -> Int -> Bool
isValidShipCoordinates coords size
  | not (isValidCoordinates (fst coords))
    || not (isValidCoordinates (snd coords)) = False
  | not $ isShipHorizontal || isShipVertical = False
  | isShipHorizontal && horizontalDiff /= size - 1 = False
  | isShipVertical && verticalDiff /= size - 1 = False
  | otherwise = True
  where isShipHorizontal  = isRangeHorizontal coords
        isShipVertical    = isRangeVertical coords
        horizontalDiff    = snd (snd coords) - snd (fst coords)
        verticalDiff      = fst (snd coords) - fst (fst coords)

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

transformGame :: Game -> Game
transformGame game = undefined

-- TODO: Game implementation
--    1. If either of the players' boards has no more Ship cells, the game is over.
--          -> Update the game state to GameOver
--    2. Otherwise return just the game as it is.
checkGameOver :: Game -> Game
checkGameOver game = undefined