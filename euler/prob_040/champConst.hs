import Data.List
import Data.Char

buildArray :: String -> [Int]
buildArray [] = []
buildArray (x:xs) = (ord x) - 48 : buildArray xs

champConst = concat $ map show [1..]

--mad easy -- just gotta index from 0 not 1
main = do
	let a = [champConst !! 0,champConst !! 9,champConst !! 99,champConst !! 999,champConst !! 9999,champConst !! 99999,champConst !! 999999]
	print $ product (buildArray a)