module Logic where

import Data.Char
import Data.List
import Data.Maybe (isJust)
import Text.Read

import Board ( Board, Cell(Unknown, Miss, Hit), emptyBoard )
import Game ( Game(state, playerOne, playerTwo, currentPlayer), boardSize, State (Running, GameOver) )
import Player ( Player(Player, name, ships, board) )
import Ship ( Ship(name, coordinates, size), Coordinates )
import Utility ( transformList, charToInt, removeItem )
import Validate ( isRangeOverlapping, isHitCell, isUnknownCell, isMissCell, horizontalDiff, isRangeHorizontal, verticalDiff, isRangeVertical, isValidCoordinates )

switchPlayer :: Game -> Game
switchPlayer game
  | currentPlayer game == playerOne game  = game { currentPlayer = playerTwo game }
  | otherwise                             = game { currentPlayer = playerOne game }

mapCellToBoard :: Cell -> Char
mapCellToBoard cell
  | isHitCell cell = 'x'
  | isUnknownCell cell = ' '
  | isMissCell cell = 'o'
  | otherwise = '?'

allShipCellCoordinates :: Ship -> [Coordinates]
allShipCellCoordinates ship
  | isRangeHorizontal $ coordinates ship =
      [(x, y) | x <- [fst $ fst $ coordinates ship], y <- [snd (fst $ coordinates ship) .. snd (snd $ coordinates ship)]]
  | isRangeVertical $ coordinates ship =
      [(x, y) | x <- [snd $ fst $ coordinates ship], y <- [fst (fst $ coordinates ship) .. fst (snd $ coordinates ship)]]

fire :: Coordinates -> Board -> [Ship] -> (Bool, Bool, [Ship], Maybe Ship, Int, Int, [Coordinates], [Cell])
fire coords board ships
  | isJust ship && shipCellsHitCount == maybe 0 size ship - 1 = (True, True, newShips ship, ship, maybe 0 size ship, shipCellsHitCount, shipCoords, shipCells)
  | isJust ship = (True, False, ships, ship, maybe 0 size ship, shipCellsHitCount, shipCoords, shipCells)
  | otherwise = (False, False, ships, Nothing, 0, 0, [], [])
  where shipsCoords = map coordinates ships
        ship = find (isRangeOverlapping (coords, coords) . coordinates) ships
        newShips Nothing = ships
        newShips (Just ship) = removeItem ship ships
        cell c = coordinatesToCell c board
        shipCoords = maybe [] allShipCellCoordinates ship
        shipCells = [cell x | x <- maybe [] allShipCellCoordinates ship]
        shipCellsHitCount = length $ filter id [isHitCell $ cell x | x <- maybe [] allShipCellCoordinates ship]

coordinatesToCell :: Coordinates -> Board -> Cell
coordinatesToCell coords board = (board !! x) !! y
  where x = fst coords
        y = snd coords

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
        yIsValid str            = isJust (readMaybe str :: Maybe Int) && stringToIntMinusOne str >= 0 && stringToIntMinusOne str <= 9
stringToCoordinates _ = (-1, -1)

coordinatesToString :: Coordinates -> String
coordinatesToString (x,y)
  | x == 0 = "A" ++ intToStringPlusOne
  | x == 1 = "B" ++ intToStringPlusOne
  | x == 2 = "C" ++ intToStringPlusOne
  | x == 3 = "D" ++ intToStringPlusOne
  | x == 4 = "E" ++ intToStringPlusOne
  | x == 5 = "F" ++ intToStringPlusOne
  | x == 6 = "G" ++ intToStringPlusOne
  | x == 7 = "H" ++ intToStringPlusOne
  | x == 8 = "I" ++ intToStringPlusOne
  | x == 9 = "J" ++ intToStringPlusOne
  | otherwise = "A0"
  where intToStringPlusOne = show $ y + 1

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