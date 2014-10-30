import Data.List
import Data.Maybe

-- after like 4 hours of trying to figure it out I gave up.

--------------------------------------------------------------------
--Faster method, found on haskell wiki 
--https://www.haskell.org/haskellwiki/Prime_numbers 
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
--------------------------------------------------------------------

triNumbAt n = (n * (n+1)) `div` 2

triNumbs = [ triNumbAt x | x <- [1..]]

numOfDivisors n = product $ map ((+1) . length) (group (primeFactors n)) 

triNumbFactors = [numOfDivisors x | x <- triNumbs]

main = do
	print $ triNumbAt $ (+1) $ fromJust $ findIndex (>500) triNumbFactors







