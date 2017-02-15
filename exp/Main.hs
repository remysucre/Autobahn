{-# LANGUAGE BangPatterns, FlexibleContexts #-}
-- import Debug.Trace

import qualified DmdParser as DP
import Preprocess

import System.Environment
import System.Process
import Language.Haskell.Exts
import Data.Generics.Uniplate.Data
import Control.Monad.State.Strict

main :: IO ()
main = do
  [fn] <- getArgs
  parOrig <- par fn
  let mainStripped = dropMain $ renameModule parOrig defaultModuleName
  writeFile "Dum.hs" (prettyPrint mainStripped)
  _ <- system "ghc -O2 Dum.hs -ddump-rn -ddump-stranal -ddump-to-file -fforce-recomp"
  fcd <- dumprn2hs "Dum"
  writeFile "Dumrn.hs" fcd

  !fc <- readFile "Dum.dump-stranal"
  let Right res = DP.parse DP.stranal "src" fc
  parres <- par "/Users/rem/Autobahn/exp/Dumrn.hs"
  let bs = binders parres
  let sbs = safePats bs res
  let (safed, _) = markSafePats sbs parres
  putStrLn $ prettyPrint safed

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
          -- trace ("NOT bang " ++ show px ++ "," ++ show p) (put ps)
          return px

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

{- Stripping Main Modules -}

defaultModuleName :: ModuleName
defaultModuleName = ModuleName "Dum"

renameModule :: Module -> ModuleName -> Module
renameModule (Module a _ b c _ d e) mn =
  Module a mn (LanguagePragma (SrcLoc "nothing" 1 1) [Ident "BangPatterns"] : b) c Nothing d e

dropMain :: Module -> Module
dropMain (Module a b c d e f decls) = Module a b c d e f decls'
  where decls' = filter (not . isMain)decls
        isMain (TypeSig _ [Ident "main"] _ )= True
        isMain (PatBind _ (PVar (Ident "main")) _ _) = True
        isMain _ = False
