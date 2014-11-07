import Data.List
import Data.Maybe

--------------------------------------------------------------------------------
--Prime boilerplate
primes = 2 : oddprimes
  where
    oddprimes = sieve 3 9 oddprimes (inits oddprimes)  -- [],[3],[3,5],...
    sieve x q ~(_:t) (fs:ft) =
      filter ((`all` fs) . ((/=0).) . rem) [x,x+2..q-2]
      ++ sieve (q+2) (head t^2) t ft

primeFactors n = factor n primes
  where
    factor n (p:ps) 
        | p*p > n        = [n]
        | n `mod` p == 0 = p : factor (n `div` p) (p:ps)
        | otherwise      =     factor n ps
--------------------------------------------------------------------------------

relPrimes :: Int -> [Int]
relPrimes 1 = [1]
relPrimes n = 1 : [x | x <- [2..(n-1)], gcd n x == 1]

coprime :: Int -> Int -> Bool
coprime a b = gcd a b == 1

uniqPrimes :: Int -> [Int]
uniqPrimes = (nub.primeFactors)

totWork :: Int -> Double
totWork x = (1- (1/(fromIntegral x)))

phi :: Int -> Int
phi n = round $ (*) (fromIntegral n) $ product $ map totWork $ uniqPrimes n

noverphin :: (Int,Int) -> Double
noverphin (n,phin) = (/) (fromIntegral n) (fromIntegral phin)

main = do
	let vals = zip ([2..1000000]) (map phi [2..1000000])
	let sols = map noverphin vals
	print $ (+2) $ fromJust $findIndex (==maximum sols) sols






