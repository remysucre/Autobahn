import System.Environment
import Data.List

main :: IO ()
main = do
  [fn] <- getArgs
  fc <- readFile fn
  let ls = lines fc
      ps_0 = groupProgs ls
  let ps_1 = reverse $ sortBySize ps_0
      ps = map fst ps_1
  print $ map snd (take 10 ps_1)
  let ss = map (sum . map countSafe) ps
  print ss
  let sl = map (sum . map countLazy) ps
  print sl
  let sn = map (sum . map countNoRec) ps
  print sn
  let total = zipWith (+) (zipWith (+) ss sl) sn
  print total
  let sortedByB = unzip4 . reverse . sortOn (\(t, _, _, _) -> t) $ zip4 total ss sl sn
  print sortedByB
  -- let cs = map countSafe ls
  -- print . length $ cs
  -- print cs
  -- let cs' = map countLazy ls
  -- print . length $ cs'
  -- print cs'
  -- let cs'' = map countNoRec ls
  -- print . length $ cs''
  -- print cs''
  -- let total = zipWith (+) (zipWith (+) cs cs') cs''
  -- print total
  -- let logs = annotations ls
  -- putStrLn . unlines $ logs

count :: (a -> Bool) -> [a] -> Int
count p = length . filter p

sortBySize :: [([String], String)] -> [([String], String)]
sortBySize = sortOn (\(p, pn) -> sum $ map length p)

sortByBangs :: [[String]] -> [[String]]
sortByBangs = sortOn (\p -> sum $ map (count (== '!')) p)

groupProgs :: [String] -> [([String], String)]
groupProgs ls@(_:_) = (l, pn):rest
  where l = takeWhile (not . isPrefixOf "START PROG") ls
        ls' = tail $ dropWhile (not . isPrefixOf "START PROG") ls
        pn = head $ dropWhile (not . isPrefixOf "START PROG") ls
        rest = groupProgs ls'
groupProgs x = [(x, "haha")]

annotations :: [String] -> [String]
annotations = filter isLog

isLog :: String -> Bool
isLog ('[':_) = True
isLog x = "START PROG" `isPrefixOf` x

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
