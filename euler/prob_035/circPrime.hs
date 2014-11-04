import Data.List
import Data.Maybe
import Test.HUnit
--------------------------------------------------------------------------------
-- Unit Tests -- Trying out unit testing --
-- Not all of the tests are relevant to the prog
--DigitArray
test1 = TestCase (assertEqual "For digitArray basic," [1,2,3] (digitArray 123) )
test2 = TestCase (assertEqual "For digitArray 1 element," [1] (digitArray 1))
test3 = TestCase (assertEqual "For digitArray 0," [] (digitArray 0))
test9 = TestCase (assertEqual "For digitArray 10," [1,0] (digitArray 10))
test10 = TestCase (assertEqual "For digitArray 010," [1,0] (digitArray 010))

--rot
test4 = TestCase (assertEqual "For rot 1234," [4,1,2,3] (rot [1,2,3,4]))
test5 = TestCase (assertEqual "For rot 100," [0,1,0] (rot [1,0,0]))
test6 = TestCase (assertEqual "For rot 1," [1] (rot [1]))


--rotBy
test7 = TestCase (assertEqual "For rotBy [1 2 3 4] 10," [3,4,1,2] (rotBy [1,2,3,4] 10))
test8 = TestCase (assertEqual "For rotBy [1] 10," [1] (rotBy [1] 43))

--circArray
test11 = TestCase (assertEqual "For circArray 1," [1] (circArray 1))
test12 = TestCase (assertEqual "For circArray 12345," [12345,23451,34512,45123,51234] (circArray 12345) )
test13 = TestCase (assertEqual "For circArray 10000," [1000,1,10,100] (circArray 1000))

test14 = TestCase (assertEqual "For isCircPrime 2," True (isCircPrime 2))
test15 = TestCase (assertEqual "For isCircPrime 34," False (isCircPrime 34))
test16 = TestCase (assertEqual "For isCircPrime 197," True (isCircPrime 197))
test17 = TestCase (assertEqual "For isCircPrime 79," True (isCircPrime 79))
test18 = TestCase (assertEqual "For isCircPrime 420073," False (isCircPrime 420073))


tests = TestList [TestLabel "test1" test1, TestLabel "test2" test2, TestLabel "test3" test3,
                  TestLabel "test4" test4, TestLabel "test5" test5, TestLabel "test6" test6,
                  TestLabel "test7" test7, TestLabel "test8" test8, TestLabel "test9" test9,
                  TestLabel "test10" test10, TestLabel "test11" test11, TestLabel "test12" test12,
                  TestLabel "test13" test13,TestLabel "test14" test14,TestLabel "test15" test15,
                  TestLabel "test16" test16,TestLabel "test17" test17,TestLabel "test18" test18]
--------------------------------------------------------------------------------


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

digitArray :: Integer -> [Integer]
digitArray 0 = []
digitArray x = digitArray (div x 10) ++ [mod x 10]

rot :: [Integer] -> [Integer]
rot x = (last x : init x)

rotBy :: [Integer] -> Integer -> [Integer]
rotBy x 0 = x
rotBy x 1 = rot x
rotBy x n = rotBy (rot x) (n-1)

primesUnder x = takeWhile (<x) primes

circArray :: Integer -> [Integer]
circArray n = take len $ map (read . take len) $ tails $ take (2*len -1) $ cycle str
    where
        str = show n
        len = length str

isPrime :: Integer -> Bool
isPrime x 
  | primeFactors x == [x] = True
  | otherwise = False

isCircPrime :: Integer -> Bool
isCircPrime x = all isPrime (circArray x)

main = do 
  print $ length $[ x| x <-(primesUnder 1000000), isCircPrime x]

