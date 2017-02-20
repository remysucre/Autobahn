module Main (integrate1D, main) where
import System.Environment
integrate1D ::
  Double -> Double -> (Double -> Double) -> Double
integrate1D (!l_aoG) u_aoH (!f_aoI)
  = let (!d_aoJ) = (u_aoH - l_aoG) / 8.0
    in
      d_aoJ
      * sum
          [(f_aoI l_aoG) * 0.5, f_aoI (l_aoG + d_aoJ),
           f_aoI (l_aoG + (2.0 * d_aoJ)), f_aoI (l_aoG + (3.0 * d_aoJ)),
           f_aoI (l_aoG + (4.0 * d_aoJ)), f_aoI (u_aoH - (3.0 * d_aoJ)),
           f_aoI (u_aoH - (2.0 * d_aoJ)), f_aoI (u_aoH - d_aoJ),
           (f_aoI u_aoH) * 0.5]
integrate2D l1_aEX u1_aEY (!l2_aEZ) (!u2_aF0) f_aF1
  = integrate1D
      l2_aEZ
      u2_aF0
      (\ (!y_aF2)
         -> integrate1D l1_aEX u1_aEY (\ x_aF3 -> f_aF1 x_aF3 y_aF2))
zark u_aF4 v_aF5
  = integrate2D
      0.0 u_aF4 0.0 v_aF5 (\ x_aF6 -> (\ y_aF7 -> x_aF6 * y_aF7))
ints = [1.0 .. ] :: [Double]
zarks = zipWith zark ints (map (2.0 *) ints)
rtotals
  = head zarks : zipWith (+) (tail zarks) rtotals
rtotal n_aIx = rtotals !! n_aIx
is = map (^ 4) ints
itotals
  = head is : zipWith (+) (tail is) itotals
itotal (!n_aIy) = itotals !! n_aIy
es = map (^ 2) (zipWith (-) rtotals itotals)
etotal (!n_aIz) = sum (take n_aIz es)
main
  = do { [range_aIA] <- getArgs;
         putStrLn $ show $ etotal $ read range_aIA }

