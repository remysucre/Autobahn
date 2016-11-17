{-# LANGUAGE RecordWildCards #-}

module DmdParser where

import qualified Text.Parsec as P
import qualified Text.Parsec.Token as PT
import Text.Parsec.Language
import Text.Parsec.Expr
import Text.Parsec ((<?>), (<|>))

-- Syntax of Simplified Demands

data Annot = Annot Binder Dmd deriving Show

type Binder = String
type Dmd = (StrDmd, UseDmd)

data StrDmd = B | S | L deriving Show
data UseDmd = U | A deriving Show

-- Parse the Demand Analysis Output

type Parser = P.Parsec String ()

parse :: Parser a -> P.SourceName -> String -> Either P.ParseError a
parse = P.parse

stranal :: Parser [Annot]
stranal = P.many (P.try expr)

expr :: Parser Annot
expr = buildExpressionParser table term
    <?> "expression"

term :: Parser Annot
term =  P.try (Annot <$> identifier <*> dmd)
    <|> P.anyToken *> expr

table = []

dmd :: Parser Dmd
dmd = brackets $ P.string "Dmd=" *> angles dmdsigs
  where dmdsigs = do strsig <- strdmd
                     P.char ','
                     usesig <- usedmd
                     return (strsig, usesig)

strdmd :: Parser StrDmd
strdmd =  P.try (P.char 'S' *> return S)
      <|> P.try (P.char 'L' *> return L)
      <|> P.try (P.char 'B' *> return B)
      <?> "strdmd"
usedmd :: Parser UseDmd
usedmd =  P.try (P.char 'A' *> return A)
      <|> P.try (P.manyTill P.anyToken (P.lookAhead $ P.char '>') *> return U)
      <?> "usedmd"

dmdDef :: LanguageDef st
dmdDef = emptyDef { PT.commentLine = "#"}

PT.TokenParser {..} = PT.makeTokenParser dmdDef
