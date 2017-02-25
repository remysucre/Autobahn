import Data.List
import System.Environment

main = do
  [fn] <- getArgs
  fc <- readFile fn
  let ls = lines fc
  let lss = groupBy (\l1 l2 -> (dropWhileEnd (/= '/') . dropWhile (/= '~') $ l1) 
                            == (dropWhileEnd (/= '/') . dropWhile (/= '~') $ l2))
                    ls
  let lss' = map (\(l:ls) -> labelDir l:l:ls) lss
  let res = unlines . concat $ lss'
  putStrLn res

labelDir l = "echo START PROG " 
          ++ (dropWhileEnd (/= '/') . dropWhile (/= '~') $ l)
          ++ ";"
