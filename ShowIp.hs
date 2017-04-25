module ShowIp where

ip :: String
ip = show a ++ show b ++ show c ++ show d
  where
    a = show 127 ++ "."
    b = show 224 ++ "."
    c = show 120 ++ "."
    d = show 12
