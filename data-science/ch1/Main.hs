import Control.Exception (catch, SomeException)
import System.Environment (getArgs)

import Text.CSV

main::IO()
main = csvFile

txtFile :: IO()
txtFile = do
  args <- getArgs
  let filename = parseArgs args
  input <- catch (readFile filename) $ \err -> print (err::SomeException) >> return ""
  print $ countWords input

csvFile :: IO()
csvFile = do
  let filename_csv = "input.csv"
  input_csv <- readFile filename_csv
  let csv = parseCSV filename_csv input_csv
  either handleError dowork csv

handleError _ = putStrLn "parsing error"
dowork = print.findOldest.tail

findOldest :: [Record] -> Record
findOldest [] = []
findOldest xs = foldl1 (\a b -> if age b > age a then b else a) xs

age :: [String] -> Int
age [] = 0
age [a,_] = toInt a
age [_] = 0

toInt :: String -> Int
toInt = read



parseArgs :: [String] -> String
parseArgs (a:_) = a
parseArgs _ = "input.txt"

countWords :: String -> [Int]
countWords input = map (length.words) (lines input)
