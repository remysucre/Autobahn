{-# LANGUAGE BangPatterns #-}
module Try where

foo x = x + 1

main = do print $ foo 3
