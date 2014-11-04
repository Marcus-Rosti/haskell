import Data.List

--eh whatever, it's slow but it works
main = do
	print $ (sort (permutations "0123456789")) !! 999999