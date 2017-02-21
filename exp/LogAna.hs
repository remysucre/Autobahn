import System.Environment
import Data.List

main = do
  [fn] <- getArgs
  fc <- readFile fn
  let ls = lines fc
  let cs = map countSafe ls
  print . length $ cs
  print cs
  let cs' = map countLazy ls
  print . length $ cs'
  print cs'
  let cs'' = map countNoRec ls
  print . length $ cs''
  print cs''
  let total = zipWith (+) (zipWith (+) cs cs') cs''
  print total
  -- let logs = annotations ls
  -- putStrLn . unlines $ logs

annotations :: [String] -> [String]
annotations = filter isLog

isLog :: String -> Bool
isLog ('[':_) = True
isLog _ = False

countSafe :: String -> Int
countSafe l = 
  let ps = read l
   in length . filter (isPrefixOf "safebang") $ ps

countLazy :: String -> Int
countLazy l = 
  let ps = read l
   in length . filter (isPrefixOf "lazydmd") $ ps

countNoRec :: String -> Int
countNoRec l = 
  let ps = read l
   in length . filter (isPrefixOf "!") $ ps
