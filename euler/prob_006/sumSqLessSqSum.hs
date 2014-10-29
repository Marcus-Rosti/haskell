sumOfSq x = sum $ map (**2) x 

sqOfSum x = (sum x) * (sum x)

main = do
	print $ sqOfSum [1..100] - sumOfSq [1..100]