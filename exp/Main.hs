{-# LANGUAGE BangPatterns #-}

import qualified DmdParser as DP
import Preprocess

import System.Environment
import System.Process
import Language.Haskell.Exts
import Language.Haskell.Exts.Syntax
import Data.Generics.Uniplate.Data

main = do 
  system "ghc -O2 Dum.hs -ddump-rn -ddump-stranal -ddump-to-file -fforce-recomp"
  fcd <- dumprn2hs "Dum"
  putStr fcd
  writeFile "Dumrn.hs" fcd

  !fc <- readFile "Dum.dump-stranal"
  -- let res0 = DP.parse DP.stranal "src" "[asdf [Dmd=<S,A>]]"
  let Right res = DP.parse DP.stranal "src" fc 
  -- -- let res1 = parse remany "as" "[a]"
  -- -- print res1
  -- print res0
  print res
  parres <- par "/home/rem/Autobahn/exp/Dumrn.hs"
  let bs = binders parres
  print bs
  let sbs = markSafePat bs res
  print $ map prettyPrint sbs


mentions :: Pat -> String -> Bool
mentions (PVar n) b = n == Ident b
mentions (PBangPat (PVar n)) b = n == Ident b
mentions _ _ = False

annotate :: Pat -> DP.Dmd -> Pat
annotate (PBangPat p)  (DP.S, _)= (PAsPat (Ident "safebang") (PBangPat p))
annotate (PBangPat p) (_, DP.A)= p -- absent, might as well take bang off
annotate p _ = p

markSafePat :: [Pat] -> [DP.Annot] -> [Pat]
markSafePat ps anns = foldl (\ps (DP.Annot b a) -> 
                              map (\p -> 
                                      if p `mentions` b 
                                      then annotate p a
                                      else p) 
                                  ps) 
                            ps anns

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
       ParseFailed loc e -> error $ show loc ++ e
