{-# LANGUAGE BangPatterns #-}
module Dum where
fib :: Int -> Integer -> Integer -> Integer
fib 0 _ b_aGK = b_aGK
fib n_aGL a_aGM !b_aGN
  = fib (n_aGL - 1) b_aGN (a_aGM + b_aGN)

