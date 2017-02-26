import System.Environment
import qualified Text.Parsec as P

type Parser = P.Parsec String ()

parse :: Parser a -> P.SourceName -> String -> Either P.ParseError a
parse = P.parse

main :: IO ()
main = do
  [fn] <- getArgs
  fc <- readFile fn
  let lg = parse parseProg "" fc
  print lg

data ProgLog = Prog FilePath [DmdLog]
  deriving Show
data DmdLog = Dmd [String]
  deriving Show

parseProg :: Parser ProgLog
parseProg = do
  _ <- P.string "START PROG"
  fn <- P.manyTill P.anyToken (P.char '\n')
  dmds <- P.many parseDmd
  return $ Prog fn dmds

parseDmd :: Parser DmdLog
parseDmd = do
  _ <- P.string "STARTLOG\n"
  dmdlog <- P.manyTill P.anyToken (P.string "ENDLOG\n")
  let ls = lines dmdlog
      annots_string = filter isLog ls
      annotes = map read annots_string
  return $ Dmd annotes

isLog :: String -> Bool
isLog ('[':_) = True
isLog  _ = False
