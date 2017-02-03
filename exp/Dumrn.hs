{-# LANGUAGE BangPatterns #-}
module Dum where
u = 0 : go (head u) (tail u)
go !a_aBi as_aBj
  = a_aBi + 1 : go (head as_aBj) (tail as_aBj)

