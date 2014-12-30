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
	-- let fp x = e**(sin x + x) * (cos x + 1)
	-- let err = 10**(-4)
	print $ gaussQuad f 0 10