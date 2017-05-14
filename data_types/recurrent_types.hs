module RecurrentTypes where

data List a = Nil | Cons a (List a) deriving Show

fromList :: List a -> [a]
fromList Nil = []
fromList (Cons x xs) = x : fromList xs

toList :: [a] -> List a
toList = foldr Cons Nil


-- *************************************** --

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

height :: Tree a -> Int
height (Leaf _) = 0
height (Node t1 t2) = 1 + max (height t1) (height t2)

size :: Tree a -> Int
size (Leaf _) = 1
size (Node t1 t2) = 1 + size t1 + size t2

avg :: Tree Int -> Int
avg t =
    let (c,s) = go t
    in s `div` c
  where
    go :: Tree Int -> (Int,Int)
    go (Leaf value) = (value, 1)
    go (Node t1 t2) = undefined
