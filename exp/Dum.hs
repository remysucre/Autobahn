{-# LANGUAGE BangPatterns #-}
module Dum where

u = 0 : go (head u) (tail u)
go !a as = a + 1 : go (head as) (tail as)

