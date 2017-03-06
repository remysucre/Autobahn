{-# LANGUAGE FlexibleContexts, DeriveDataTypeable #-}
import Debug.Trace

import qualified DmdParser as DP
import Preprocess

import System.Environment
-- import System.Process
import Language.Haskell.Exts
import Data.Generics.Uniplate.Data
-- import Data.List
import Data.Generics
import Control.Monad.State.Strict

data Safe = Safe | Lazy | NoRecord | Remove
  deriving (Show, Data)

main :: IO ()
main = do
  [fn] <- getArgs
  let fpOrig = fn ++ ".hs"
      fpOpt = fn ++ ".hs.opt"
      fpDmd = fn ++ ".dump-stranal"
  m0 <- par fpOrig
  print . length . binders $ m0
  fcd <- dumprn2hs fn
  writeFile "Dumrn.hs" fcd
  m <- par "Dumrn.hs"
  -- get a list of safety for all patterns
  annots <- getDmd fn
  print annots
  -- get a bit vector showing all bangs
  bs <- readBangs fpOpt
  print $ length bs
  let dmds = markWithAnnotes m annots
      bsSafety = safeBangs bs dmds
  print dmds
  print $ length dmds
  print bsSafety
  -- compare the two and return
    -- 1. a list of safety for bangs
    -- 2. a module with bangs annotated with safety

getDmd :: FilePath -> IO [DP.Annot]
getDmd fp = do
  anaFc <- readFile $ fp ++ ".dump-stranal"
  let Right res = DP.parse DP.stranal "src" anaFc
  return res

markWithAnnotes :: Module -> [DP.Annot] -> [Maybe DP.Annot]
markWithAnnotes m dmds = bsAnn
  where bs = binders m
        bsAnn = map (findAnn dmds) bs
        findAnn (d@(DP.Annot n _) : nds) b
          | b `mentions` n = Just d
          | otherwise = findAnn nds b
        findAnn [] _ = Nothing

mentions :: Pat -> String -> Bool
mentions (PVar (Ident n)) s = s == n
mentions _ _ = False

safeBangs :: [Bool] -> [Maybe DP.Annot] -> [Maybe Safe]
safeBangs = zipWith bangSafety
  where bangSafety False _ = Nothing
        bangSafety True Nothing = Just NoRecord
        bangSafety True (Just (DP.Annot s (DP.S, _))) = trace s Just Main.Safe
        bangSafety True (Just (DP.Annot _ (_, DP.A))) = Just Remove
        bangSafety True (Just (DP.Annot _ (_, _))) = Just Lazy

markSafeBangs :: [Maybe Safe] -> Module -> (Module, [Maybe Safe])
markSafeBangs ss m = runState (transformBiM go m) ss
  where go :: (MonadState [Maybe Main.Safe] m) => Pat -> m Pat
        go p = do
          (s:rest) <- get
          put rest
          case s
            of Nothing -> return p
               (Just Main.Safe) -> return (PAsPat (Ident "safebang") p)
               (Just Lazy) -> return (PAsPat (Ident "lazydmd") p)
               (Just NoRecord) -> trace (show p) $ return (PAsPat (Ident "norecord") p)
               (Just Remove) -> return (PAsPat (Ident "unused") p)

binders :: Module -> [Pat]
binders modl = bs
  where (_, bs) = runState (transformBiM go modl) []
        go :: (MonadState [Pat] m) => Pat -> m Pat
        go p = do
          ps <- get
          put $ ps ++ [p]
          return p

bangParseMode :: String -> ParseMode
bangParseMode path = defaultParseMode
  { parseFilename = path
  , baseLanguage = Haskell2010
  , extensions = [EnableExtension BangPatterns]
  , ignoreLanguagePragmas = False
  , ignoreLinePragmas = False
  , fixities = Nothing
  , ignoreFunctionArity = False
  }

par :: String -> IO Module
par path = do
  pres <- parseFileWithMode (bangParseMode path) path
  case pres of
       ParseOk res -> return res
       ParseFailed srcloc e -> error $ show srcloc ++ e

readBangs :: String -> IO [Bool]
readBangs path = do
  res <- parseFileWithMode (bangParseMode path) path
  case res of
    ParseFailed _ e -> error e
    ParseOk a       -> return . findBangs . binders $ a

findBangs :: [Pat] -> [Bool]
findBangs = foldr addBang []
  where addBang (PBangPat _) bs = True:bs
        addBang p bs
          | hasBang p = bs
          | otherwise = False:bs

hasBang :: Pat -> Bool
hasBang p = any isBang $ children p
  where isBang (PBangPat _) = True
        isBang (PParen (PBangPat _)) = True
        isBang _ = False

findPats :: Data a => a -> [Pat]
findPats = universeBi
