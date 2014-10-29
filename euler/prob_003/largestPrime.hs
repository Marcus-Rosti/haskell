import Data.List
import Data.List.Ordered (minus)

-- Seive it up // got this from stackoverflow
primes = euler [2..]
euler (p : xs) = p : euler (xs `minus` map (*p) (p : xs))


--find the factors // (p:ps) == primes = true
factor n (p:ps) 
	| p > n  = []
	| (mod) n p == 0 = p : factor (n `div` p) (p:ps)
	| otherwise = factor n ps


--obvious 
primeFactors n = factor n primes

main = do
	print $ primeFactors 600851475143
