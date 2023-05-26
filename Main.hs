import Game
import Logic
import Player
import Control.Monad
import GHC.IO.Exception
import System.Environment
import System.Process

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

playerTurn :: Game -> IO ()
playerTurn game = do
  when (shouldClearTerminal game) clearTerminal
  putStrLn (name (currentPlayer game) ++ "'s turn!")
  printBoard $ currentPlayer game
  line <- getLine
  if line == "quit"
    then do return ()
  else
    playerTurn $ switchPlayer game

main :: IO ()
main = do
  args <- getArgs
  putStrLn "Welcome to Haskellship!"
  putStrLn "Let's begin by getting the names of both players."
  names <- getNames
  let players = [Player $ head names, Player $ names!!1]
  putStrLn $ "Ok "++ name (head players)  ++ " and " ++ name (players!!1) ++ ", let's begin!"
  playerTurn $ initialGame players $ "noclear" `notElem` args

  -- TODO: Game implementation
  --    1. Initialize empty board (10x10) for each player
  --    2. Place ships on the board using coordinates (x,y)
  --    3. Take turns at firing to a set of coordinates on the board
  --    4. Once all of the ships are destroyed from either player, end the game
  --    5. Declare one player as a winner