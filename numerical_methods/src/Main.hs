import Ch_02
--import Ch_03
import Ch_06

e :: Double
e = exp 1

x_0 :: Double
x_0 = 0

main :: IO ()
main = do
	let f x = e**(sin x + x)
	let fp x = e**(sin x + x) * (cos x + 1)
	let err = 10**(-4)
	--print $ fromJust $ bisection f ((-1.0), 1.0) err
	print $ forwardDiff f x_0 err
	print $ (fp x_0) + err
	print $ (<=) (forwardDiff f x_0 err) ((fp x_0) + err)
	print $ (>=) (forwardDiff f x_0 err) ((fp x_0) - err)
	print $ centralDiff f x_0 err
	print $ (<=) (centralDiff f x_0 err) ((fp x_0) + err**2)
	print $ (>=) (centralDiff f x_0 err) ((fp x_0) - err**2)
	--print $ gaussQuad fp (-1.0) 100