{-# LANGUAGE BangPatterns #-}
module Dum where

-- Fibonnacci sequence with accum. param.
fib :: Int -> Integer -> Integer -> Integer
fib 0 _ b = b
fib n a !b = fib (n - 1) b (a + b)
