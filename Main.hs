import Control.Concurrent

getNames :: IO [String]
getNames = do
  putStrLn "Let's begin by getting the names of both players."
  putStrLn "Player 1, please enter your name:"
  playerOneName <- getLine
  putStrLn $ "Hello " ++ playerOneName ++ "!"
  threadDelay 2000000
  putStrLn "Player 2, please enter your name:"
  playerTwoName <- getLine
  putStrLn $ "Hello " ++ playerTwoName ++ "!"
  threadDelay 2000000
  return [playerOneName, playerTwoName]

greeting :: IO ()
greeting = do
  putStrLn "Welcome to Haskellship!"

main :: IO ()
main = do
  greeting

  names <- getNames
  putStrLn $ "Ok " ++ head names ++ " and " ++ last names ++ ", let's begin!"

  -- TODO: Game implementation
  --    1. Initialize empty field (10x10)
  --    2. Place ships on the board using coordinates (x, y)
  --    3. Take turns at firing to a set of coordinates on the board
  --    4. Once all of the ships are destroyed from either player, end the game