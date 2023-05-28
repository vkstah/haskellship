import Control.Concurrent ( threadDelay )
import Control.Monad ( unless, when )
import Data.List ( find )
import GHC.IO.Exception ()
import System.Environment ( getArgs )
import System.Process ( system )

import Board (emptyBoard)
import Game
import Logic ( isValidShipCoordinates, switchPlayer, stringToCoordinates )
import Player ( Player(Player, name, ships) )
import Ship ( Coordinates, Ship )

type Coordinate = (Int, Int)

clearTerminal :: IO ()
clearTerminal = do
  system "clear"
  return ()

printSuccess :: String -> IO()
printSuccess str = do
  putStrLn $ "\ESC[32m" ++ str ++ "\ESC[0m"

printError :: String -> IO ()
printError str = do
  putStrLn $ "\ESC[31m" ++ str ++ "\ESC[0m"

getPlayerInput :: String -> IO String
getPlayerInput str = do
  putStrLn str
  getLine

printBoard :: Player -> IO ()
printBoard player = do
  putStrLn "---Board here---"

getNames :: IO [String]
getNames = do
  playerOneName <- getPlayerInput "Player 1, please enter your name:"
  putStrLn $ "Hello " ++ playerOneName ++ "!"
  playerTwoName <- getPlayerInput "Player 2, please enter your name:"
  putStrLn $ "Hello " ++ playerTwoName ++ "!"
  return [playerOneName, playerTwoName]

printTurnCountdown :: Int -> IO Bool
printTurnCountdown seconds = do
  if seconds == 0
    then do return True
    else do
      putStrLn $ "\ESC[94mSwitching players in " ++ show seconds ++ "...\ESC[0m"
      threadDelay 1000000
      printTurnCountdown (seconds - 1)

getCoordinatesRange :: String -> IO (Coordinates, Coordinates)
getCoordinatesRange str = do
  coordsLine <- getPlayerInput str
  let coords = words coordsLine
  if length coords == 2
    then do return (stringToCoordinates $ head coords, stringToCoordinates $ coords !! 1)
    else do
      printError "Invalid ship range."
      getCoordinatesRange str

getShip :: String -> Int -> IO Ship
getShip name size = do
  range <- getCoordinatesRange $ "Please enter the coordinates of your " ++ name ++ " (" ++ show size ++ " cells):"
  if isValidShipCoordinates range size
    then do return range
    else do
      printError "Invalid ship size."
      getShip name size

getShips :: String -> IO [Ship]
getShips name = do
  putStrLn $ name ++ ": place your ships!"
  sequence [getShip name size | (name, size) <- shipTypes]

playerTurn :: Game -> IO ()
playerTurn game = do
  when (shouldClearTerminal game) clearTerminal
  let player = currentPlayer game
  putStrLn (name player ++ "'s turn!")
  printBoard player
  line <- getLine
  printSuccess "You sunk a Battleship!"
  printTurnCountdown 3
  playerTurn $ switchPlayer game

main :: IO ()
main = do
  args <- getArgs
  putStrLn "Welcome to Haskellship!"
  putStrLn "Let's begin by getting the names of both players."
  names <- getNames
  putStrLn "Alright! Moving onto the ships..."
  let playerOneName = head names
  playerOneShips <- getShips playerOneName
  let playerTwoName = names!!1
  let playerOneBoard = emptyBoard
  let playerOne = Player playerOneName playerOneShips playerOneBoard
  playerTwoShips <- getShips playerTwoName
  let playerTwoBoard = emptyBoard
  let playerTwo = Player playerTwoName playerTwoShips playerTwoBoard
  let players = (playerOne, playerTwo)
  playerTurn $ initialGame players $ "noclear" `notElem` args

  -- TODO: Game implementation
  --    1. Initialize empty board (10x10) for each player
  --    2. Place ships on the board using coordinates (x,y)
  --    3. Take turns at firing to a set of coordinates on the board
  --    4. Once all of the ships are destroyed from either player, end the game
  --    5. Declare one player as a winner