module SumTypes where

data Color = Red | Green | Blue

instance Show Color where
  show Red = "Red"
  show Green = "Green"
  show Blue = "Blue"


data LogLevel = Error | Warning | Info

cmp :: LogLevel -> LogLevel -> Ordering
cmp Error Error = EQ
cmp Warning Warning = EQ
cmp Info Info = EQ
cmp Error _ = GT
cmp _ Error = LT
cmp Info _  = LT
cmp _ Info  = GT


data Result = Fail | Success

processData :: SomeData -> String
processData someData =
  case doSomeWork someData of
    (_, 0) -> "Success"
    (_, err) -> "Fail: " ++ show err
