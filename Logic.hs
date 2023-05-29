module Logic where

import Data.Char
import Data.List
import Data.Maybe (isJust)

import Board ( Board, Cell(Empty, Miss, Hit), emptyBoard )
import Game ( Game(state, playerOne, playerTwo, currentPlayer), boardSize, State (Running, GameOver) )
import Player ( Player(Player, name, ships, board) )
import Ship ( Ship(name, coordinates, size), Coordinates )
import Utility ( transformList, charToInt, removeItem )
import Validate ( isRangeOverlapping, isHitCell, isEmptyCell, isMissCell, horizontalDiff, isRangeHorizontal, verticalDiff, isRangeVertical )

switchPlayer :: Game -> Game
switchPlayer game
  | currentPlayer game == playerOne game  = game { currentPlayer = playerTwo game }
  | otherwise                             = game { currentPlayer = playerOne game }

mapCellToBoard :: Cell -> Char
mapCellToBoard cell
  | isHitCell cell = 'x'
  | isEmptyCell cell = ' '
  | isMissCell cell = 'o'
  | otherwise = '?'

allShipCellCoordinates :: Ship -> [Coordinates]
allShipCellCoordinates ship
  | isRangeHorizontal $ coordinates ship =
      [(x, y) | x <- [fst $ fst $ coordinates ship], y <- [snd (fst $ coordinates ship) .. snd (snd $ coordinates ship)]]
  | isRangeVertical $ coordinates ship =
      [(x, y) | x <- [snd $ fst $ coordinates ship], y <- [fst (fst $ coordinates ship) .. fst (snd $ coordinates ship)]]

-- TODO: Check if ship was completely destroyed in a guard
-- | and [isRangeOverlapping (coords, coords) (x, x) | x <- maybe [] allShipCellCoordinates ship] = (True, True, newShips ship, ship)
fire :: Coordinates -> [Ship] -> (Bool, Bool, [Ship], Maybe Ship)
fire coords ships
  | and [isRangeOverlapping (coords, coords) (x, x) | x <- maybe [] allShipCellCoordinates ship] = (True, True, newShips ship, ship)
  | or [isRangeOverlapping (coords, coords) x | x <- shipsCoords] && isJust ship = (True, False, ships, ship)
  | otherwise = (False, False, ships, Nothing)
  where shipsCoords = map coordinates ships
        ship = find (isRangeOverlapping (coords, coords) . coordinates) ships
        newShips Nothing = []
        newShips (Just ship) = removeItem ship ships

opponentPlayer :: Game -> Player
opponentPlayer game
  | currentPlayer game == playerOne game  = playerTwo game
  | otherwise                             = playerOne game

stringToCoordinates :: String -> Coordinates
stringToCoordinates [x,y]
  | not $ yIsValid y = (-1, -1)
  | x == 'A' = (0, charToInt y - 1)
  | x == 'B' = (1, charToInt y - 1)
  | x == 'C' = (2, charToInt y - 1)
  | x == 'D' = (3, charToInt y - 1)
  | x == 'E' = (4, charToInt y - 1)
  | x == 'F' = (5, charToInt y - 1)
  | x == 'G' = (6, charToInt y - 1)
  | x == 'H' = (7, charToInt y - 1)
  | x == 'I' = (8, charToInt y - 1)
  | x == 'J' = (9, charToInt y - 1)
  | otherwise = (-1, -1)
  where yIsValid c = isDigit c && charToInt c - 1 >= 0 && charToInt c - 1 <= 9
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
stringToCoordinates _ = (-1, -1)

transformGame :: Coordinates -> Bool -> [Ship] -> Game -> Game
transformGame coords hit newShips game
  | currentPlayer game == playerOne game =
      switchPlayer
      $ checkGameOver newShips
      $ game { playerTwo = markShot coords hit originalPlayerTwo }
  | otherwise =
      switchPlayer
      $ checkGameOver newShips
      $ game { playerOne = markShot coords hit originalPlayerOne }
  where originalPlayerOne = playerOne game
        originalPlayerTwo = playerTwo game

markShot :: Coordinates -> Bool -> Player -> Player
markShot coords hit player
  | hit       = player { board = markHit coords (board player) }
  | otherwise = player { board = markMiss coords (board player) }

markMiss :: Coordinates -> Board -> Board
markMiss (x,y) board = transformList x (transformList y Miss (board!!x)) board

markHit :: Coordinates -> Board -> Board
markHit (x,y) board = transformList x (transformList y Hit (board!!x)) board

updateShips :: [Ship] -> Player -> Player
updateShips newShips player = player { ships = newShips }

checkGameOver :: [Ship] -> Game -> Game
checkGameOver newShips game
  | null newShips = game { state = GameOver }
  | currentPlayer game == playerOne game = game { playerTwo = updateShips newShips $ playerTwo game }
  | currentPlayer game == playerTwo game = game { playerOne = updateShips newShips $ playerOne game }