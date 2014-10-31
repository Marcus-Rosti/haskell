-- buildArray :: String -> [Int]
-- buildArray [] = []
-- buildArray (x:xs) = (C.ord x) - 48 : buildArray xs

--I really hate IO

getNumbers file = fmap (map read . lines) (readFile file)

firstTen = (take 10 . show . sum)

main = do
 	numbs <- getNumbers "num.txt"
	print $ firstTen numbs
