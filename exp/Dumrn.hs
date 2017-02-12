{-# LANGUAGE BangPatterns #-}
module Dum where
u = 0 : go (head u) (tail u)
go !a_aBl as_aBm
  = a_aBl + 1 : go (head as_aBm) (tail as_aBm)

