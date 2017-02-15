{-# LANGUAGE BangPatterns #-}
module Dum where
import Data.Ratio
import System.Environment
powers :: [[Integer]]
powers
  = [2 .. ] : map (zipWith (*) (head powers)) powers
neg_powers :: [[Integer]]
neg_powers
  = map
      (zipWith
         (\ n_aO2 x_aO3 -> if n_aO2 then x_aO3 else - x_aO3)
         (iterate not True))
      powers
pascal :: [[Integer]]
pascal
  = [1, 2, 1]
    : map
        (\ line_a1JL -> zipWith (+) (line_a1JL ++ [0]) (0 : line_a1JL))
        pascal
bernoulli 0 = 1
bernoulli 1 = - (1 % 2)
bernoulli n_a1Tk | odd n_a1Tk = 0
bernoulli n_a1Tl
  = (- 1) % 2
    + sum
        [fromIntegral
           ((sum $ zipWith (*) powers_a1Tm (tail $ tail combs_a2c3))
            - fromIntegral k_a2c2)
         % fromIntegral (k_a2c2 + 1) |
           (k_a2c2, combs_a2c3) <- zip [2 .. n_a1Tl] pascal]
  where
      powers_a1Tm = (neg_powers !! (n_a1Tl - 1))

