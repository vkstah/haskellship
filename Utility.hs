module Utility where

import Data.Char ( digitToInt )

transformList :: (Eq a, Num a, Enum a) => a -> b -> [b] -> [b]
transformList x y = zipWith (\ i v -> (if i == x then y else v)) [0 .. ]

charToInt :: Char -> Int
charToInt c = digitToInt c :: Int

removeItem :: Eq a => a -> [a] -> [a]
removeItem _ []                 = []
removeItem x (y:ys)
  | x == y    = removeItem x ys
  | otherwise = y : removeItem x ys