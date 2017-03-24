module Discount where
  discount :: Double -> Double -> Double -> Double
  discount limit proc s = if s >= limit then s * (100 - proc) / 100 else s

  standardDiscount :: Double -> Double
  standardDiscount = discount 1000 5
