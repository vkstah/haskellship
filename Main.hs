import Control.Concurrent

import Game

type Coordinate = (Int, Int)

getPlayerInput :: String -> IO String
getPlayerInput str = do
  putStrLn str
  getLine

getNames :: IO [String]
getNames = do
  playerOneName <- getPlayerInput "Player 1, please enter your name:"
  putStrLn $ "Hello " ++ playerOneName ++ "!"
  threadDelay 2000000
  playerTwoName <- getPlayerInput "Player 2, please enter your name:"
  putStrLn $ "Hello " ++ playerTwoName ++ "!"
  threadDelay 2000000
  return [playerOneName, playerTwoName]

main :: IO ()
main = do
  putStrLn "Welcome to Haskellship!"
  putStrLn "Let's begin by getting the names of both players."
  names <- getNames
  putStrLn $ "Ok " ++ head names ++ " and " ++ last names ++ ", let's begin!"
  start []

  -- TODO: Game implementation
  --    1. Initialize empty board (10x10) for each player
  --    2. Place ships on the board using coordinates (x,y)
  --    3. Take turns at firing to a set of coordinates on the board
  --    4. Once all of the ships are destroyed from either player, end the game
  --    5. Declare one player as a winner