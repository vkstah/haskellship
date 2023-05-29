module Utility where

transformList :: (Eq a, Num a, Enum a) => a -> b -> [b] -> [b]
transformList x y = zipWith (\ i v -> (if i == x then y else v)) [0 .. ]