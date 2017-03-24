module Fib where

fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fib2 n
    | n == 0 = 0
    | helper n 1 0

helper n res prev
    | n == 0 = res
    | otherwise = helper (n - 1) (res + prev) res
