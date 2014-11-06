import Data.List

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
        | n `mod`  p == 0 = p : factor (n `div` p) (p:ps)
        | otherwise      =     factor n ps
--------------------------------------------------------------------------------

isPrime :: Integer -> Bool
isPrime n = (primeFactors n) == [n]

primesUnder :: Integer -> [Integer]
primesUnder x = takeWhile (<x) primes

primeSumUpTo :: Int -> Integer
primeSumUpTo n = sum (take n primes)

main = do
  let lowPrimes = primesUnder 1000
  let primeSums = takeWhile (<1000) $ map primeSumUpTo [0..]
  --print $ maximum [x | x <- primeSums, isPrime x]
  print $ take 21 primes
