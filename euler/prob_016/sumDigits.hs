import Data.Char

--mad easy
sumDigits = sum . map digitToInt . show

main = do
	print $ sumDigits (2^1000)