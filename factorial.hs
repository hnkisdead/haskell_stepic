module Factorial where

  factorial :: Int -> Int
  factorial 0 = 1
  factorial n = n * factorial (n - 1)

  doubleFact :: Int -> Int
  doubleFact (-1) = 1
  doubleFact 0 = 1
  doubleFact n = n * doubleFact (n - 2)

  factorialTail :: Int -> Int
  factorialTail n
    | n >= 0    = helper 1 n
    | otherwise = error "arg must be >=0"

  helper :: Int -> Int -> Int
  helper acc 0 = acc
  helper acc n = helper (acc * n) (n - 1)
