import Data.List
import Control.Concurrent

--Faster method, found on haskell wiki 
--https://www.haskell.org/haskellwiki/Prime_numbers 
primes = 2 : oddprimes
  where
    oddprimes = sieve 3 9 oddprimes (inits oddprimes)  -- [],[3],[3,5],...
    sieve x q ~(_:t) (fs:ft) =
      filter ((`all` fs) . ((/=0).) . rem) [x,x+2..q-2]
      ++ sieve (q+2) (head t^2) t ft

primesLessThan x = takeWhile (<x) primes

main = do
	print $ sum $ primesLessThan 2000000