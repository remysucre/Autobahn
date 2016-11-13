{-# LANGUAGE BangPatterns #-}

module Soundness where

import Rewrite
import System.Process

checkBangs :: String -> IO [(Bool, [Bool])]
checkBangs path = do 
  bs <- readBangs $ path ++ ".hs"
  print bs
  diffBangs bs path
  where 
    diffBangs bs src = 
      sequence $ map (\bs' -> ((,) <$> ((==) <$> dmdAnal bs' <*> dmdAnal bs)
                                    <*> return bs')) (bsMutants bs)
    dmdAnal bs = do
          putStrLn $ "bangs are " ++ show bs
          fc <- editBangs (path ++ ".hs") bs 
          writeFile (path ++ "opt.hs") fc 
          system $ "ghc -O2 -ddump-stranal -ddump-to-file -fforce-recomp " ++ path ++ "opt"
          log <- readFile $ path ++ "opt" ++ ".dump-stranal"
          let log' = unlines . drop 4 . lines $ log
          system $ "diff -y " ++ path ++ ".dump-stranal " ++ path ++ "opt.dump-stranal"
          system $ "rm " ++ path ++ "opt" ++ ".dump-stranal"
          return log
    bsMutants bs = mutate (length bs) bs
    mutate 0 bs = [bs]
    mutate n bs = flipb n bs : mutate (n - 1) bs
    flipb 1 (b:bs) = not b : bs
    flipb n (b:bs) = b : flipb (n-1) bs
