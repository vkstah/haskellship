module Player where

import Ship ( Ship )

data Player = Player
  { name :: String
  , ships :: [Ship]
  } deriving (Eq, Show)

printBoard :: Player -> IO ()
printBoard player = do
  putStrLn "---Board here---"