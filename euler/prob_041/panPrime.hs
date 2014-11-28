import Data.List
import Data.Char

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

intArr :: Int -> [Int]
intArr = map digitToInt . show

sortedDigits = sort . intArr

isPanNum :: Int -> Bool
isPanNum x = sortedDigits x == [1..length (show x)]

isPrime :: Int -> Bool
isPrime n = (length . primeFactors) n == 1

strToInt :: String -> Int
strToInt x = read x :: Int

main = do 
  let perms = permutations "123456789" ++ permutations "12345678" ++ permutations "1234567" ++ permutations "123456"
  let realNums = map strToInt perms
  print $ maximum $ filter isPrime realNums