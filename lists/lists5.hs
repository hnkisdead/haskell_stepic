module Lists where

meanList :: [Double] -> Double
meanList = uncurry (/) . foldr (\x (s, len) -> (x + s, len + 1)) (0, 0)

evenOnly :: [a] -> [a]
evenOnly = undefined

evenOnlyFoldr :: [a] -> ([(a, Integer)], Integer)
evenOnlyFoldr = foldr (\x (s, pos) -> ((x, pos) : s, pos + 1)) ([], 1)

evenOnlyFoldl :: [a] -> [a]
evenOnlyFoldl = fst . foldl f ([], 1)
  where f (s, pos) x | even pos = (s ++ [x], pos + 1)
                     | otherwise = (s, pos + 1)
