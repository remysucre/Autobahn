{-# LANGUAGE BangPatterns, FlexibleContexts #-}

import qualified DmdParser as DP
import Preprocess

import System.Environment
import Language.Haskell.Exts
import Language.Haskell.Exts.Syntax
import Data.Generics.Uniplate.Data
import Control.Monad
import Control.Monad.State.Strict

main = do 
  fc <- dumprn2hs "Dum"
  putStr fc
  orig <- par "/home/rem/Autobahn/exp/Dum.hs"

  !fc <- readFile "test.dump"
  -- let res0 = DP.parse DP.stranal "src" "[asdf [Dmd=<S,A>]]"
  let Right res = DP.parse DP.stranal "src" fc 
  -- -- let res1 = parse remany "as" "[a]"
  -- -- print res1
  -- print res0
  print res
  parres <- par "/home/rem/Autobahn/exp/Dumrn.hs"
  let bs = binders parres
  print bs
  let sbs = safePats bs res
  let (safed, _) = markSafePats sbs orig
  putStrLn $ prettyPrint safed


mentions :: Pat -> String -> Bool
mentions (PVar n) b = n == Ident b
mentions (PBangPat (PVar n)) b = n == Ident b
mentions _ _ = False

annotate :: Pat -> DP.Dmd -> Pat
annotate (PBangPat p)  (DP.S, _)= (PAsPat (Ident "safebang") (PBangPat p))
annotate (PBangPat p) (_, DP.A)= p -- absent, might as well take bang off
annotate p _ = p

safePats :: [Pat] -> [DP.Annot] -> [Pat]
safePats ps anns = foldl (\ps (DP.Annot b a) -> 
                              map (\p -> 
                                      if p `mentions` b 
                                      then annotate p a
                                      else p) 
                                  ps) 
                            ps anns

markSafePats :: [Pat] -> Module -> (Module, [Pat])
markSafePats sps x = runState (transformBiM go x) sps
  where go :: (MonadState [Pat] m, Uniplate Pat) => Pat -> m Pat
        go pb@(PBangPat p) = do 
           (p:ps) <- get
           put ps
           case p 
             of (PAsPat (Ident "safebang") (PBangPat _)) -> return (PAsPat (Ident "safebang") pb)
                _ -> return pb
        go x = return x

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
  ParseOk res <- parseFileWithMode (bangParseMode path) path
  return res
