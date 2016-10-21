module Soundness where

import Rewrite
import System.Process

checkBangs :: String -> IO [(Bool, [Bool])]
checkBangs path = do 
  bs <- readBangs path
  diffBangs bs path
  where 
    diffBangs bs src = 
      sequence $ map (\bs' -> ((,) <$> ((==) <$> dmdAnal bs' <*> dmdAnal bs)
                                    <*> return bs')) (bsMutants bs)
    dmdAnal bs = do
          system $ "rm " ++ path ++ ".dump-stranal"
          fc <- editBangs path bs 
          writeFile fc path
          system $ "ghc -O2 -ddump-stranal -fforce-recomp" ++ path
          log <- readFile $ path ++ ".dump-stranal"
          return log
    bsMutants bs = mutate (length bs) bs
    mutate 0 bs = [bs]
    mutate n bs = flipb n bs : mutate (n - 1) bs
    flipb 1 (b:bs) = not b : bs
    flipb n (b:bs) = b : flipb (n-1) bs
