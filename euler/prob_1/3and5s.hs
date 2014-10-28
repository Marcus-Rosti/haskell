import Data.List


--recursion
myFunc::[Int] -> Int
myFunc (x:xs)
	| xs == [] = x
	| x `mod` 3 == 0 = (+) x (myFunc xs)
	| x `mod` 5 == 0 = (+) x (myFunc xs)
	| otherwise = (+) 0 (myFunc xs)

main = do
	--print $ myFunc [1..999]
	-- #haskellUpInDisB || I think this is the better way...
	print $ sum [ x | x <- [1..999], (mod) x 3 == 0 || (mod) x 5 == 0]
