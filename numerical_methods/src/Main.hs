import Ch_02
--import Ch_03
import Ch_06

main :: IO ()
main = do
	let f x = cos (x) +x
	-- let fp x = e**(sin x + x) * (cos x + 1)
	let err = 10**(-5)
	let a_0 = 0
	let b_0 = pi+1
	print $ gaussQuad f a_0 b_0
	print $ adaptiveQuad f a_0 b_0 err