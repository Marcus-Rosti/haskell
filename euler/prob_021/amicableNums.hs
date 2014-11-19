divisors :: Integer -> [Integer]
divisors n = 1 : filter ((==0) . rem n) [2 .. n `div` 2]

sumOfDivisors:: Integer -> Integer
sumOfDivisors n = sum (divisors n)

amicable :: Integer -> Bool
amicable n = (sumOfDivisors . sumOfDivisors) n == n && n /= sumOfDivisors n

main = do
	--print $ [x | x <- [1..10000], amicable x]
	print $ sum $ filter (amicable) [1..10000]