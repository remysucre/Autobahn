{-# LANGUAGE BangPatterns #-}
module Dum where
fib :: Int -> Integer -> Integer -> Integer
fib 0 _ b_aHt = b_aHt
fib n_aHu a_aHv !b_aHw
  = fib (n_aHu - 1) b_aHw (a_aHv + b_aHw)

