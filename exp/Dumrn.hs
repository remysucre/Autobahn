{-# LANGUAGE BangPatterns #-}
module Dum where
import Data.Complex
import System.Environment
f :: Int -> Complex Double
f (!n_a2Kt)
  = mkPolar 1 ((2 * pi) / fromIntegral n_a2Kt) ^ n_a2Kt

