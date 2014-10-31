selfPow :: Integer -> Integer
selfPow x = (^) x x

takeLast n = reverse.(take n).reverse.show
main = do
	let bigNum = sum $ map selfPow [1..1000]
	print $ takeLast 10 bigNum