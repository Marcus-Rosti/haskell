import Data.List
import Data.Maybe

relPrimes :: Int -> [Int]
relPrimes 1 = [1]
relPrimes n = 1 : [x | x <- [2..(n-1)], gcd n x == 1]

coprime :: Int -> Int -> Bool
coprime a b = gcd a b == 1

totient n = length $ filter (coprime n) [1..(n-1)]

phi n = length $ relPrimes n

noverphin :: (Int,Int) -> Double
noverphin (n,phin) = (/) (fromIntegral n) (fromIntegral phin)

main = do
	let vals = zip ([2..1000000]) (map totient [2..1000000])
	let sols = map noverphin vals
	print $ (+2) $ fromJust $findIndex (==maximum sols) sols