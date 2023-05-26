import Control.Concurrent ( threadDelay )
import Control.Monad ( when )
import GHC.IO.Exception ()
import System.Environment ( getArgs )
import System.Process ( system )

import Game
import Logic ( isValidShipCoordinates, switchPlayer, stringToCoordinates )
import Player ( Player(Player, name), printBoard )
import Ship ( Ship )

type Coordinate = (Int, Int)

clearTerminal :: IO ()
clearTerminal = do
  system "clear"
  return ()

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

printTurnCountdown :: Int -> IO Bool
printTurnCountdown seconds = do
  if seconds == 0
    then do return True
    else do
      putStrLn $ "Switching players in " ++ show seconds ++ "..."
      threadDelay 1000000
      printTurnCountdown (seconds - 1)

getShip :: String -> Int -> IO Ship
getShip name size = do
  putStrLn $ "Please enter the beginning coordinates of your " ++ name ++ " (" ++ show size ++ " cells):"
  beginningCoords <- getLine
  putStrLn $ "Please enter the end coordinates of your " ++ name ++ " (" ++ show size ++ " cells):"
  endCoords <- getLine
  let coords = (stringToCoordinates beginningCoords, stringToCoordinates endCoords)
  if isValidShipCoordinates coords
    then do return coords
    else do getShip name size

getShips :: String -> IO [Ship]
getShips name = do
  putStrLn $ name ++ ": place your ships!"
  ships <- sequence [getShip name size | (name, size) <- shipTypes]
  return []

playerTurn :: Game -> IO ()
playerTurn game = do
  when (shouldClearTerminal game) clearTerminal
  putStrLn (name (currentPlayer game) ++ "'s turn!")
  printBoard $ currentPlayer game
  line <- getLine
  if line == "quit"
    then do return ()
    else do
      putStrLn "You sunk a Battleship!"
      printTurnCountdown 5
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
  playerTwoShips <- getShips playerOneName
  let players = [Player playerOneName playerOneShips, Player playerTwoName playerTwoShips]
  putStrLn $ "Ok "++ name (head players)  ++ " and " ++ name (players!!1) ++ ", let's begin!"
  playerTurn $ initialGame players $ "noclear" `notElem` args

  -- TODO: Game implementation
  --    1. Initialize empty board (10x10) for each player
  --    2. Place ships on the board using coordinates (x,y)
  --    3. Take turns at firing to a set of coordinates on the board
  --    4. Once all of the ships are destroyed from either player, end the game
  --    5. Declare one player as a winner