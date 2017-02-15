{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BangPatterns #-}
module Dum where
f [] (!c) = c
f (!((!(x : xs)))) (!c) = f xs (uncurry (tick x) c)
tick x (!c0) (!c1)
  | even x = (c0, c1 + 1)
  | otherwise = (c0 + 1, c1)