module Lists where

import Data.Char

readDigits :: String -> (String, String)
readDigits = span isDigit

filterDisj :: (a -> Bool) -> (a -> Bool) -> [a] -> [a]
filterDisj _ _ [] = []
filterDisj p1 p2 (x:xs)
  | p1 x || p2 x = x : filterDisj p1 p2 xs
  | otherwise    = filterDisj p1 p2 xs

squaresNCubes :: Num a => [a] -> [a]
squaresNCubes = concatMap (\x -> [x * x, x * x * x * x])
