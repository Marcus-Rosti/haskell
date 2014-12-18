import Ch_02
--import Ch_03
import Ch_06
import Data.Maybe

e :: Double
e = 2.7182818284590452353602874713527

main :: IO ()
main = do
	let f x = x
	let fp x = 1
	let err = 10**(-10)
	print $ fromJust $ bisection f ((-1.0), 1.0) err
	print $ forwardDiff f 1.0 err
	print $ centralDiff f 16 err
	print $ gaussQuad fp 0 100