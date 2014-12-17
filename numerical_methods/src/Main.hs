import Ch_02
import Ch_03
import Ch_06
import Data.Maybe

e :: Double
e = 2.7182818284590452353602874713527

main :: IO ()
main = do
	let f x = e**x + x
	let fp x = e**x + 1
	let err = 10**(-15)
	print $ fromJust $ bisection f ((-1.0), 1.0) err