{-# LANGUAGE BangPatterns #-}
module Dum where

{-
f x = let !y = x + 1 in (case y of 1 -> True
                                  _  -> False)

fun x = let fun y = y + x in fun 1

-}
f !f_c f_p = case f_p 
          of (f_a, f_b) -> if f_c
                       then (f_a, True)
                       else f_p

thing p s = () --case p of (a, b) -> p
{- useH (x, y) = ()
lazyAbs x = ()
yAbs !x = ()
strictUse x = x
strictUseTup (x, y) = (x, y) -}
