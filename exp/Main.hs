{-# LANGUAGE BangPatterns, FlexibleContexts #-}

import qualified DmdParser as DP
import Preprocess

import System.Process
import Language.Haskell.Exts
import Data.Generics.Uniplate.Data
import Control.Monad.State.Strict

main :: IO ()
main = do
  _ <- system "ghc -O2 Dum.hs -ddump-rn -ddump-stranal -ddump-to-file -fforce-recomp"
  fcd <- dumprn2hs "Dum"
  -- putStr fcd
  writeFile "Dumrn.hs" fcd

  !fc <- readFile "Dum.dump-stranal"
  -- let res0 = DP.parse DP.stranal "src" "[asdf [Dmd=<S,A>]]"
  let Right res = DP.parse DP.stranal "src" fc
  -- -- let res1 = parse remany "as" "[a]"
  -- -- print res1
  -- print res0
  print res
  parres <- par "/Users/rem/Autobahn/exp/Dumrn.hs"
  putStrLn $ prettyPrint parres
  let bs = binders parres
  print $ map prettyPrint bs
  let sbs = safePats bs res
  print $ map prettyPrint sbs
  print $ sbs
  print $ length sbs
  -- let sbs = map (PVar . Ident) ["a", "b", "c", "d"]
  let (safed, safebangs) = markSafePats sbs parres
  print $ map prettyPrint safebangs
  putStrLn $ prettyPrint safed
  let sfbs = binders safed
  print $ map prettyPrint sfbs
  print $ length sfbs
  print $ zip (map prettyPrint sfbs) (map prettyPrint sbs)


mentions :: Pat -> String -> Bool
mentions (PVar n) b = n == Ident b
mentions (PBangPat (PVar n)) b = n == Ident b
mentions _ _ = False

annotate :: Pat -> DP.Dmd -> Pat
annotate (PBangPat p)  (DP.S, _)= PAsPat (Ident "safebang") (PBangPat p)
annotate (PBangPat p) (_, DP.A)=  PAsPat (Ident "remove") (PBangPat p) -- absent, might as well take bang off
annotate (PBangPat p) (_, _)=  PAsPat (Ident "investigate") (PBangPat p) -- absent, might as well take bang off
annotate p _ = p

safePats :: [Pat] -> [DP.Annot] -> [Pat]
safePats = foldl (\ps (DP.Annot b a) ->
                     map (\p ->
                             if p `mentions` b
                             then annotate p a
                             else p)
                     ps)

markSafePats :: [Pat] -> Module -> (Module, [Pat])
markSafePats sps x = runState (transformBiM go x) sps
  where go :: (MonadState [Pat] m) => Pat -> m Pat
        go pb@(PBangPat pv) = do
          (p:ps) <- get
          put ps
          case p
            of (PAsPat (Ident "safebang") _) -> return pb
               (PAsPat (Ident "remove") _) -> return (PAsPat (Ident "removed") pv)
               -- (PBangPat _) -> return (PAsPat (Ident "investigate") pv)
               _ -> return (PAsPat (Ident "investigate") pv)
        go px = do
          (_:ps) <- get
          put ps
          return px

binders :: Module -> [Pat]
binders = universeBi

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
