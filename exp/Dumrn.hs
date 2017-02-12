{-# LANGUAGE BangPatterns #-}
module Dum where
u = 0 : go (head u) (tail u)
go !a_aBj as_aBk
  = a_aBj + 1 : go (head as_aBk) (tail as_aBk)

