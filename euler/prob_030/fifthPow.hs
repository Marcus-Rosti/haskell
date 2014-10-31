digitArray:: Int -> [Int]
digitArray 0 = []
digitArray x = digitArray (div x 10) ++ [mod x 10]

fifthPow:: [Int] -> Int
fifthPow x = sum $ map (^5) x

fifthPowDig :: Int -> Int 
fifthPowDig = fifthPow.digitArray

main = do
	let nums = [x | x <- [2..1000000], x == fifthPowDig x]
	print $ sum nums