module Lists where

nTimes :: a -> Int -> [a]
nTimes e n = helper e n [] where
  helper _ 0 acc      = acc
  helper el times acc = helper el (times - 1) (el : acc)

oddsOnly :: Integral a => [a] -> [a]
oddsOnly [] = []
oddsOnly (x:xs)
  | odd x = x : oddsOnly xs
  | otherwise = oddsOnly xs

sum3 :: Num a => [a] -> [a] -> [a] -> [a]
sum3 (x:xs) (y:ys) (z:zs) = x + y + z : sum3 xs ys zs
sum3 (x:xs) []     []     = x : sum3 xs [] []
sum3 []     (y:ys) []     = y : sum3 [] ys []
sum3 []     []     (z:zs) = z : sum3 [] [] zs
sum3 []     (y:ys) (z:zs) = y + z : sum3 [] ys zs
sum3 (x:xs) []     (z:zs) = x + z : sum3 xs [] zs
sum3 (x:xs) (y:ys) []     = x + y : sum3 xs ys []
sum3 []     []     []     = []
