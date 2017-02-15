{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BangPatterns #-}
module Dum where
f [] (!c_aql) = c_aql
f (!((!(x_aqm : xs_aqn)))) (!c_axs)
  = f xs_aqn (uncurry (tick x_aqm) c_axs)
tick x_axF (!c0_axG) (!c1_axH)
  | even x_axF = (c0_axG, c1_axH + 1)
  | otherwise = (c0_axG + 1, c1_axH)

