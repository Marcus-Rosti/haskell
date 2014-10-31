import Data.Char

buildArray :: String -> [Int]
buildArray [] = []
buildArray (x:xs) = (ord x) - 48 : buildArray xs

main = do
	let facString = show $ product [1..100]
	print $ (sum.buildArray) facString