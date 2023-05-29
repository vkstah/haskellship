import Control.Concurrent ( threadDelay )
import Control.Monad ( unless, when )
import Data.List ( find )
import GHC.IO.Exception ()
import System.Environment ( getArgs )
import System.Process ( system )

import Board ( Board, emptyBoard, Cell(Empty, Hit, Miss) )
import Game
import Logic ( fireHitShip, isValidShipCoordinates, isValidCoordinatesRange, switchPlayer, stringToCoordinates, mapCellToBoard, markHit, markMiss, opponentPlayer, isValidCoordinates )
import Player ( Player(Player, name, ships, board) )
import Ship ( Coordinates, Ship(Ship) )

type Coordinate = (Int, Int)

clearTerminal :: IO ()
clearTerminal = do
  system "clear"
  return ()

printSuccess :: String -> IO()
printSuccess str = putStrLn $ "\ESC[32m" ++ str ++ "\ESC[0m"

printError :: String -> IO ()
printError str = putStrLn $ "\ESC[31m" ++ str ++ "\ESC[0m"

printNotification :: String -> IO ()
printNotification str = putStrLn $ "\ESC[35m" ++ str ++ "\ESC[0m"

printTurnCountdown :: Int -> IO Bool
printTurnCountdown seconds = do
  if seconds == 0
    then do return True
    else do
      putStrLn $ "\ESC[94mSwitching players in " ++ show seconds ++ "...\ESC[0m"
      threadDelay 1000000
      printTurnCountdown (seconds - 1)

printBoard :: Board -> IO ()
printBoard board = do
  let cells = [map mapCellToBoard row | row <- board]
  putStrLn $ replicate 12 'H'
  mapM_ (\row -> putStrLn $ "H" ++ row ++ "H") cells
  putStrLn $ replicate 12 'H'

getPlayerInput :: String -> IO String
getPlayerInput str = do
  putStrLn str
  getLine

getNames :: IO [String]
getNames = do
  playerOneName <- getPlayerInput "Player 1, please enter your name:"
  putStrLn $ "Hello " ++ playerOneName ++ "!"
  playerTwoName <- getPlayerInput "Player 2, please enter your name:"
  putStrLn $ "Hello " ++ playerTwoName ++ "!"
  return [playerOneName, playerTwoName]

getCoordinates :: String -> IO Coordinates
getCoordinates str = do
  coordsLine <- getPlayerInput str
  let coordsList = words coordsLine
  if length coordsList == 1
    then do
      let coords = stringToCoordinates $ head coordsList
      if isValidCoordinates coords
        then do return coords
        else do
          printError "Invalid coordinates."
          getCoordinates str
    else do
      printError "Invalid coordinates."
      getCoordinates str

getCoordinatesRange :: String -> IO (Coordinates, Coordinates)
getCoordinatesRange str = do
  coordsLine <- getPlayerInput str
  let coordsList = words coordsLine
  if length coordsList == 2
    then do
      let coordsRange =
            (stringToCoordinates $ head coordsList,
            stringToCoordinates $ coordsList !! 1)
      if isValidCoordinatesRange coordsRange
        then do return coordsRange
        else do
          printError "Invalid coordinates range."
          getCoordinatesRange str
    else do
      printError "Invalid coordinates range."
      getCoordinatesRange str

getShip :: String -> Int -> [Ship] -> IO Ship
getShip name size currentShips = do
  range <- getCoordinatesRange $ "Please enter the coordinates of your " ++ name ++ " (" ++ show size ++ " cells):"
  if isValidShipCoordinates range size currentShips
    then do
      return $ Ship name range size
    else do
      printError "Invalid ship size."
      getShip name size currentShips

getShips :: [(String, Int)] -> [Ship] -> IO [Ship]
getShips [] _                   = return []
getShips (x:xs) currentShips    = do
  ship <- uncurry getShip x currentShips
  ships <- getShips xs (ship : currentShips)
  return $ ship : ships

fire :: Game -> IO Game
fire game = undefined

playerTurn :: Game -> IO ()
playerTurn game = do
  when (shouldClearTerminal game) clearTerminal
  let player = currentPlayer game
  let opponent = opponentPlayer game
  putStrLn (Player.name player ++ "'s turn!")
  putStrLn ""
  printBoard (board opponent)
  putStrLn ""
  fireCoords <- getCoordinates "Enter coordinates to fire:"
  if state game == GameOver
    then do
      printNotification $ Player.name player ++ " won the game!"
    else do
      putStrLn ""
      printTurnCountdown 3
      playerTurn $ switchPlayer game

main :: IO ()
main = do
  args <- getArgs
  putStrLn "Welcome to Haskellship!"
  putStrLn "Let's begin by getting the names of both players."
  names <- getNames
  let playerOneName = head names
  let playerOneBoard = emptyBoard
  putStrLn $ playerOneName ++ ": place your ships!"
  playerOneShips <- getShips shipTypes initialShips
  let playerOne = Player playerOneName playerOneShips playerOneBoard
  let playerTwoName = names!!1
  let playerTwoBoard = emptyBoard
  putStrLn $ playerTwoName ++ ": place your ships!"
  playerTwoShips <- getShips shipTypes initialShips
  let playerTwo = Player playerTwoName playerTwoShips playerTwoBoard
  let players = (playerOne, playerTwo)
  playerTurn $ initialGame players $ "noclear" `notElem` args

  -- TODO: Game implementation
  --    1. Initialize empty board (10x10) for each player
  --    2. Place ships on the board using coordinates (x,y)
  --    3. Take turns at firing to a set of coordinates on the board
  --    4. Once all of the ships are destroyed from either player, end the game
  --    5. Declare one player as a winner