buildArray :: String -> [Int]
buildArray [] = []
buildArray (x:xs) = (ord x) - 48 : buildArray xs

main = do
	let num =<< getContents "num.txt"
	print $ num