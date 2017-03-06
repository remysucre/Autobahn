module Main (main) where
import System.Environment
infix 8 ^^^

int :: Nat -> Int
int Z = 0
int (S x_aLo) = 1 + int x_aLo
x_aLp ^^^ Z = S Z
x_aLq ^^^ (S y_aLr) = x_aLq * (x_aLq ^^^ y_aLr)
main
  = do { [power_aLs] <- getArgs;
         print $ int (3 ^^^ (fromInteger $ read power_aLs)) }

data Nat
  = Z | S Nat
  deriving (Eq, Ord, Show)

instance Num Nat where
  Z + y_aPZ = y_aPZ
  (S x_aQ0) + y_aQ1 = S (x_aQ0 + y_aQ1)
  x_aQ2 * Z = Z
  x_aQ3 * (S y_aQ4) = x_aQ3 * y_aQ4 + x_aQ3
  fromInteger x_aQ5
    = if x_aQ5 < 1 then Z else S (fromInteger (x_aQ5 - 1))

