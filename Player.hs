module Player where

newtype Player = Player
  { name :: String
  } deriving (Eq, Show)

printBoard :: Player -> IO ()
printBoard player = do
  putStrLn (name player ++ "'s board:")
  putStrLn "---Board here---"