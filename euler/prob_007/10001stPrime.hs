import Data.List
import Data.List.Ordered (minus)

-- Seive it up // got this from stackoverflow
primes = euler [2..]
euler (p : xs) = p : euler (xs `minus` map (*p) (p : xs))

primeAtIndex i = primes !! (i-1)

--hmmm this is slow... I bet there's a faster way!
main = do
	print $ primeAtIndex 10001