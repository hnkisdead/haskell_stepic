module Seq where

  seqA :: Integer -> Integer
  seqA n
    | n == 0 = 1
    | n == 1 = 2
    | n == 2 = 3
    | otherwise = let
        helper acc 0 1 2 = acc
        helper acc n = helper (acc )
      in helper 0 n
