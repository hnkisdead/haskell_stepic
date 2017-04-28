module Lists where

import           Data.Char

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

readDigits :: String -> (String, String)
readDigits = span isDigit

filterDisj :: (a -> Bool) -> (a -> Bool) -> [a] -> [a]
filterDisj _ _ [] = []
filterDisj p1 p2 (x:xs)
  | p1 x || p2 x = x : filterDisj p1 p2 xs
  | otherwise    = filterDisj p1 p2 xs


squaresNCubes :: Num a => [a] -> [a]
squaresNCubes = concatMap (\x -> [x * x, x * x * x * x])

fibStream :: [Integer]
fibStream = 1 : fibStream


data Odd = Odd Integer
  deriving (Eq, Show)

addEven :: Odd -> Integer -> Odd
addEven (Odd n) m | m `mod` 2 == 0 = Odd (n + m)
                  | otherwise      = error "addEven: second parameter cannot be odd"

oddToInteger :: Odd -> Integer
oddToInteger (Odd n) = n

instance Enum Odd where
  succ x = addEven x 2
  pred x = addEven x (-2)
  fromEnum x = fromInteger $ oddToInteger x
  toEnum x
    | x `mod` 2 == 1 = Odd $ toInteger x
    | otherwise      = error "toEnum: "


max3 :: Ord a => [a] -> [a] -> [a] -> [a]
max3 = zipWith3 (\x y z -> max x $ max y z)

meanList :: [Double] -> Double
meanList = uncurry (/) . foldr (\x (s, len) -> (x + s, len + 1)) (0, 0)

evenOnly :: [a] -> [a]
evenOnly = fst . foldr (\x (l, pos) -> if even pos then (x : l, pos + 1) else (l, pos + 1)) ([], 0)

evenOnly' :: [a] -> [a]
evenOnly' = fst . foldr evenPos ([], 0) where
  evenPos x (l, pos) | even pos = (x : l, pos + 1)
                     | otherwise = (l, pos + 1)
