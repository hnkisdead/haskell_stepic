module Fib where

  fib :: Int -> Int
  fib 0 = 0
  fib 1 = 1
  fib n = fib (n - 1) + fib (n - 2)

  fib2 :: Integer -> Integer
  fib2 n
    | n == 0 = 0
    | n == 1 = 1
    | n == (-1) = 1
    | n > 0 = helper n 1 0
    | otherwise = helper n (-1) 0

  helper :: Integer -> Integer -> Integer -> Integer
  helper n res prev
    | n == 0 = res
    | n == 1 = res
    | n == (-1) = res
    | n > 0 = helper (n - 1) (res + prev) res
    | otherwise = helper (n + 1) (res - prev) res

  fibonacci :: Integer -> Integer
  fibonacci 0 = 0
  fibonacci 1 = 1
  fibonacci (-1) = 1
  fibonacci n | n > 0 = fibonacci (n - 1) + fibonacci (n - 2)
              | otherwise = fibonacci (n + 2) - fibonacci (n + 1)
