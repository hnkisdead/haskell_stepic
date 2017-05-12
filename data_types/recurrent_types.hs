module RecurrentTypes where

data List a = Nil | Cons a (List a) deriving Show

fromList :: List a -> [a]
fromList Nil = []
fromList (Cons x xs) = x : fromList xs

toList :: [a] -> List a
toList = foldr Cons Nil


-- *************************************** --

data Tree a = Leaf a | Node (Tree a) (Tree a)

height :: Tree a -> Int
height = undefined

size :: Tree a -> Int
size = undefined
