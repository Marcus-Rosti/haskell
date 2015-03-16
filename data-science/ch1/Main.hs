import Control.Exception (catch, SomeException)
import System.Environment (getArgs)

main::IO()
main=do
  args <- getArgs
  let filename = case args of
    (a:_) -> a
    _     -> "input.txt"
  input <- readFile filename
  print $ countWords input

countWords :: String -> [Int]
countWords input = map (length.words) (lines input)
