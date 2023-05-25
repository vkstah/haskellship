import Game
import Logic

type Coordinate = (Int, Int)

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


-- TODO: Game implementation
--    1. If either of the players' boards has no more Ship cells, the game is over.
--          -> Update the game state to GameOver
--    2. Otherwise return just the game as it is.
checkGameOver :: Game -> Game
checkGameOver game = undefined

start :: Game -> IO ()
start game = do
  putStrLn (name (currentPlayer game) ++ "'s turn!")
  line <- getLine
  if line == "quit"
      then do return ()
    else
      start $ switchPlayer game

main :: IO ()
main = do
  putStrLn "Welcome to Haskellship!"
  putStrLn "Let's begin by getting the names of both players."
  names <- getNames
  let players = [Player $ head names, Player $ names!!1]
  let game = Game Running players (head players)
  putStrLn $ "Ok "++ name (head players)  ++ " and " ++ name (players!!1) ++ ", let's begin!"
  start game

  -- TODO: Game implementation
  --    1. Initialize empty board (10x10) for each player
  --    2. Place ships on the board using coordinates (x,y)
  --    3. Take turns at firing to a set of coordinates on the board
  --    4. Once all of the ships are destroyed from either player, end the game
  --    5. Declare one player as a winner