{-# LANGUAGE BangPatterns #-}
module Dum where
fib :: Int -> Integer -> Integer -> Integer
fib 0 _ b_aGO = b_aGO
fib n_aGP a_aGQ !b_aGR
  = fib (n_aGP - 1) b_aGR (a_aGQ + b_aGR)

