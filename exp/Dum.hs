{-# LANGUAGE BangPatterns #-}
module Dum where
import Data.Complex
import System.Environment

f :: Int -> Complex Double
f (!n) = mkPolar 1 ((2 * pi) / fromIntegral n) ^ n