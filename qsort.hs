module QSort where

qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort [x] = [x]
qsort (x:xs) = qsort ltex ++ [x] ++ qsort gtx
  where ltex = filter (<= x) xs
        gtx = filter (> x) xs
