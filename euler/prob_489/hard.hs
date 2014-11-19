import Control.Parallel.Strategies

up_to :: Integer
up_to = 10000000

asAnInte :: Integer
asAnInte = 3

g :: Integer -> Integer -> Integer
g a b = maximum $ [ gcd (n^asAnInte + b) ((n+a)^asAnInte + b) | n <- [1..up_to] ]
--g a b = maximum $ parMap rseq (\n -> gcd (n^asAnInte + b) ((n+a)^asAnInte + b)) [1..up_to]

h :: Integer -> Integer -> Integer
h m n = foldr1 (+) [ g a b | a <- [1..m] , b <- [1..n]]


main :: IO ()
main = do
	print $ h 5 5