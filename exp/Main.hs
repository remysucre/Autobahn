import DmdParser

main = do 
  let res = parse expr "src" "hahalol [Dmd=<S,U>]"
  print res
