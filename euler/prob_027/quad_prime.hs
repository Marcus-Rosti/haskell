isPrime :: Integer -> Bool
isPrime x = not $ any divisible $ takeWhile notTooBig [2..] where
     divisible y = x `mod`y == 0
     notTooBig y = y*y <= x

allPositive x = not $ any (<0) x

allPrime :: [Integer] -> Bool
allPrime x = not $ any (not.isPrime) x


main :: IO ()
main = do
	--print $ [(a,b) | a<-[-1000..1000],b<-[-1000..1000], allPrime [n*n+a*n+b| n<-[0..abs (a-1)] ] ]
	let a = (-937)
	let b = (-645)
	print $ allPrime [n*n+a*n+b| n<-[0..abs (a-1)] ]