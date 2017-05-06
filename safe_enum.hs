module SafeEnum where

class (Enum a, Bounded a, Eq a) => SafeEnum a where
  ssucc :: a -> a
  spred :: a -> a

  ssucc p
    | p == maxBound = minBound
    | otherwise = succ p

  spred p
    | p == minBound = maxBound
    | otherwise = pred p

instance SafeEnum Bool where

instance SafeEnum Int where
