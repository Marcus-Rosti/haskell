phi = (/) (1.0 + sqrt 5.0 ) 2.0 :: Double

fib x = round $ phi ** fromIntegral x / sqrt 5

tenMil = 10000000

-- I bet I could have done this better
-- TODO: maybe with sum again?
sumFib x 
	| fib x < tenMil && mod (fib x) 2 == 0 = fib x +sumFib (x+1)
	| fib x < tenMil = sumFib (x+1)
	| otherwise = 0

main = do
	print $ sumFib 0