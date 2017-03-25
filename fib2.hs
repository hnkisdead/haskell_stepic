module Fib2 where

  fib2 :: Integer -> Integer
  fib2 n
    | n == 0 = 0
    | n == 1 = 1
    | otherwise = helper n 1 0
    where
      helper n' res prev
        | n' == 0 = res
        | otherwise = helper (n' - 1) (res + prev) res
