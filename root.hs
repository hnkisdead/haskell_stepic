module Root where

  roots :: Int -> Int -> Int -> (Float, Float)
  roots a b c =
    let d = sqrt (b ^ 2 - 4 * a * c) in
    ((-b - d) / (2 * a), (-b + d) / (2 * a))
