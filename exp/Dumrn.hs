{-# LANGUAGE BangPatterns #-}
module Dum where
f [] (!c_aqo) = c_aqo
f (!((!(x_aqp : xs_aqq)))) (!c_axv)
  = f xs_aqq (uncurry (tick x_aqp) c_axv)
tick x_axI (!c0_axJ) (!c1_axK)
  | even x_axI = (c0_axJ, c1_axK + 1)
  | otherwise = (c0_axJ + 1, c1_axK)

