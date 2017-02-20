{-# LANGUAGE BangPatterns #-}
module Dum where
returnM :: x_azx -> Maybe x_azx
returnM x_azy = Just x_azy
eachM :: Maybe x_azv -> (x_azv -> y_azw) -> Maybe y_azw
eachM ((!(Just x_azz))) (!f_azA) = Just (f_azA x_azz)
eachM Nothing (!f_azB) = Nothing
thenM :: Maybe x_azt -> (x_azt -> Maybe y_azu) -> Maybe y_azu
thenM ((!(Just x_azC))) kM_azD = kM_azD x_azC
thenM Nothing (!kM_azE) = Nothing
failM :: Maybe x_azs
failM = Nothing
orM :: Maybe x_azr -> Maybe x_azr -> Maybe x_azr
orM (Just x_azF) yM_azG = Just x_azF
orM Nothing (!yM_azH) = yM_azH
guardM :: Bool -> Maybe x_azq -> Maybe x_azq
guardM (!b_azI) (!xM_azJ) = if b_azI then xM_azJ else failM
filterM :: (x_azp -> Bool) -> Maybe x_azp -> Maybe x_azp
filterM p_azK xM_azL
  = xM_azL
    `thenM`
      (\ (!x_azM) -> p_azK x_azM `guardM` returnM x_azM)
theM :: Maybe x_azo -> x_azo
theM (!((!(Just x_azN)))) = x_azN
existsM :: Maybe x_asj -> Bool
existsM ((!(Just x_azO))) = True
existsM Nothing = False
useM :: x_asi -> Maybe x_asi -> x_asi
useM xfail_azP ((!(Just (!x_azQ)))) = x_azQ
useM xfail_azR (!Nothing) = xfail_azR

