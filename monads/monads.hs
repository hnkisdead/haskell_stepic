module Monads where

data Log a = Log [String] a deriving Show


toLogger :: (a -> b) -> String -> (a -> Log b)
toLogger f msg = Log [msg] . f

execLoggers :: a -> (a -> Log b) -> (b -> Log c) -> Log c
execLoggers x f g =
  case f x of
    (Log l x1) ->
      case g x1 of
        (Log l1 x2) -> Log (l ++ l1) x2

returnLog :: a -> Log a
returnLog = Log []

bindLog :: Log a -> (a -> Log b) -> Log b
bindLog (Log l x) f = undefined

add1Log :: Integer -> Log Integer
add1Log = toLogger (+1) "added one"
