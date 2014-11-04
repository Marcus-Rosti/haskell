-- fast fact
fact :: Integer -> Integer
fact 0 = 1
fact n = product [1..n]

--formula for n choose r
nchooser :: Integer -> Integer -> Integer
nchooser n r = (fact n) `quot` ((fact r) * (fact (n-r)))

--list comprehension
main = do
	print $ length [(n,r) | n <-[1..100], r <- [1..n], (nchooser n r)>999999]