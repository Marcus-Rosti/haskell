-- fast fact
fact :: Integer -> Integer
fact 0 = 1
fact n = product [1..n]

--formula for n choose r
nchooser :: Integer -> Integer -> Integer
nchooser n r = (fact n) `quot` ((fact r) * (fact (n-r)))


{-
The number of paths of length a+b from the origin (0,0) to a point (a,b)
 which are restricted to east and north steps
  is given by the binomial coefficient (a+b; a)
-}
main = do
	print $ nchooser 40 20