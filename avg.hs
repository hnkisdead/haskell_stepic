module Avg where

avg :: Int -> Int -> Int -> Double
avg a1 a2 a3 = (fromIntegral a1 + fromIntegral a2 + fromIntegral a3) / 3
