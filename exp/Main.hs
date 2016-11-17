import DmdParser

import System.Environment

main = do 
  fc <- readFile "test.dump"
  let res0 = parse stranal "src" "[asdf [Dmd=<S,A>]]"
  let res = parse stranal "src" fc 
  let res1 = parse remany "as" "[a]"
  print res1
  print res0
  print res
