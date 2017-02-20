module Main (main) where
import Data.Complex
import System.Environment
main
  = do { [arg_axG] <- getArgs;
         print
           (round
              (realPart (sum [f n_aRg | n_aRg <- [1 .. (read arg_axG)]]))) }
f :: Int -> Complex Double
f (!n_aRh) = mkPolar 1 ((2 * pi) / fromIntegral n_aRh) ^ n_aRh

