module Lists where

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
