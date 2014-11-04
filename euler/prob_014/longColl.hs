import Data.List

--hella slow!
--Memoization would be best but I don't want to deal with
--the overhead of that right now
--TODO:: make this faster
collatzSize (num, size)
	| num == 1 = size
	| (mod) num 2 == 0 = collatzSize (quot num 2, size+1)
	| otherwise = collatzSize (3 * num + 1, size+1)

main = do
	let nums = zip [1..999999] [0,0..]
	let collNums = map collatzSize nums
	print $ map (+1) (findIndices (== (maximum collNums) ) (collNums))


