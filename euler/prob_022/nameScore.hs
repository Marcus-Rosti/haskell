import Data.Char
import Data.List.Split
import Data.List
import Data.Maybe

scoreLetter :: Char -> Int
scoreLetter letter = (ord letter) - 64

scoreWord :: [Char] -> Int
scoreWord word = sum $ map scoreLetter word

scoreSorted :: [[Char]] -> [Int]
scoreSorted x = map (\(x,y) -> x*y) $zip [1..] $ map scoreWord x


main = do 
	myFile <- readFile "names.txt"
	let lightlyParsed = splitOn "\"" myFile
	let fullyParsed = filter (/= "") $ filter (/= ",") lightlyParsed
	let sorted = sort fullyParsed
	print $ sum $ scoreSorted sorted
