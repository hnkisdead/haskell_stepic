module GorkAndMork where

class KnownToGork a where
    stomp :: a -> a
    doesEnrageGork :: a -> Bool

class KnownToMork a where
    stab :: a -> a
    doesEnrageMork :: a -> Bool

class (KnownToGork a, KnownToMork a) => KnownToGorkAndMork a where
    stompOrStab :: a -> a
    stompOrStab p
      | doesEnrageGork p = stomp p
      | doesEnrageMork p = stab p
      | doesEnrageGork p && doesEnrageMork p = stomp (stab p)
      | otherwise = p
