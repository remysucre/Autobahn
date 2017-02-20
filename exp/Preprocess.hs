{-# LANGUAGE BangPatterns #-}

module Preprocess (dumprn2hs)
where

import Data.List
import Language.Haskell.Exts

dumprn2hs :: String -> IO String
dumprn2hs fp = do
  ParseOk (Module a b c d e f _) <- parseFileWithMode (bangParseMode $ fp ++ ".hs") (fp ++ ".hs")
  let fc0 = prettyPrint (Module a b c d e f [])
  let fn = takeWhile (/= '.') . reverse . takeWhile (/= '/') . reverse $ fp
  fc <- readFile $ fp ++ ".dump-rn"
  putStrLn fc
  let fc' =  replace (fn ++ ".") "" fc
  return . (fc0 ++) . ("\n" ++) . unlines . drop 2 . lines $ fc'

replace :: (Eq a) => [a] -> [a] -> [a] -> [a]
replace _ _ [] = []
replace old new xs@(y:ys) =
  case stripPrefix old xs of
      Nothing -> y : replace old new ys
      Just ys' -> new ++ replace old new ys'

bangParseMode :: String -> ParseMode
bangParseMode !path = defaultParseMode
  { parseFilename = path
  , baseLanguage = Haskell2010
  , extensions = [EnableExtension BangPatterns]
  , ignoreLanguagePragmas = False
  , ignoreLinePragmas = False
  , fixities = Nothing
  , ignoreFunctionArity = False
  }
