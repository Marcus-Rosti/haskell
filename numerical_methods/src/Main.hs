--import Ch_02
--import Ch_03
import Ch_06

main :: IO ()
main = do
	let f x = cos (x) + x
	let antiF x = (sin x) + 0.5 * x ** 2.0
	-- let fp x = e**(sin x + x) * (cos x + 1)
	let err = 10**(-5)
	let a_0 = 0
	let b_0 = 200*pi+1
	print $ gaussQuad f a_0 b_0
	print $ adaptiveQuad f a_0 b_0 err
	print $ antiF b_0 - antiF a_0