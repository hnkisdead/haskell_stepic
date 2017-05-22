module Functor where

data Point3D a = Point3D a a a deriving Show

instance Functor Point3D where
  fmap f (Point3D x1 x2 x3) = Point3D (f x1) (f x2) (f x3)


-- ******************************* --

data GeomPrimitive a
    = Point (Point3D a)
    | LineSegment (Point3D a) (Point3D a)
    deriving Show

instance Functor GeomPrimitive where
  fmap f (Point p) = Point (fmap f p)
  fmap f (LineSegment p1 p2) = LineSegment (fmap f p1) (fmap f p2)


-- ******************************* --

data Tree a
    = Leaf (Maybe a)
    | Branch (Tree a) (Maybe a) (Tree a)
    deriving Show

instance Functor Tree where
  fmap f (Leaf x) = Leaf (fmap f x)
  fmap f (Branch l x r) = Branch (fmap f l) (fmap f x) (fmap f r)


-- ******************************* --

data Entry k1 k2 v = Entry (k1, k2) v  deriving Show
data Map k1 k2 v = Map [Entry k1 k2 v]  deriving Show

instance Functor (Entry k1 k2) where
  fmap f (Entry (x1, x2) v) = Entry (x1, x2) (f v)

instance Functor (Map k1 k2) where
  fmap f (Map xs) = Map (map (fmap f) xs)
