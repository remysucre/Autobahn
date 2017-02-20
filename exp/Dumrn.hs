{-# LANGUAGE BangPatterns #-}
module Dum where
import System.Environment
integrate1D :: Double -> Double -> (Double -> Double) -> Double
integrate1D (!l_azc) u_azd (!f_aze)
  = let (!d_azf) = (u_azd - l_azc) / 8.0
    in
      d_azf
      * sum
          [(f_aze l_azc) * 0.5, f_aze (l_azc + d_azf),
           f_aze (l_azc + (2.0 * d_azf)), f_aze (l_azc + (3.0 * d_azf)),
           f_aze (l_azc + (4.0 * d_azf)), f_aze (u_azd - (3.0 * d_azf)),
           f_aze (u_azd - (2.0 * d_azf)), f_aze (u_azd - d_azf),
           (f_aze u_azd) * 0.5]
integrate2D l1_a12X u1_a12Y (!l2_a12Z) (!u2_a130) f_a131
  = integrate1D
      l2_a12Z
      u2_a130
      (\ (!y_a132)
         -> integrate1D
              l1_a12X u1_a12Y (\ x_a133 -> f_a131 x_a133 y_a132))
zark u_a134 v_a135
  = integrate2D
      0.0 u_a134 0.0 v_a135 (\ x_a136 -> (\ y_a137 -> x_a136 * y_a137))
ints = [1.0 .. ] :: [Double]
zarks = zipWith zark ints (map (2.0 *) ints)
rtotals
  = head zarks : zipWith (+) (tail zarks) rtotals
rtotal n_a16O = rtotals !! n_a16O
is = map (^ 4) ints
itotals = head is : zipWith (+) (tail is) itotals
itotal (!n_a16P) = itotals !! n_a16P
es = map (^ 2) (zipWith (-) rtotals itotals)
etotal (!n_a16Q) = sum (take n_a16Q es)

