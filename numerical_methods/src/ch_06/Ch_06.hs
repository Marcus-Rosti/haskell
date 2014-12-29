module Ch_06 (forwardDiff, centralDiff, secondDir, lagrange, trapazoid, rienmannSums, gaussQuad) where

import Data.List

-- differentiation 
forwardDiff :: 	(Double -> Double)	-- ^ f(x)
				-> Double 			-- ^ x_0
				-> Double 			-- ^ error
				-> Double			-- ^ f'(x_0)
forwardDiff f x0 h = ((f (x0 + h)) - (f (x0))) / h

centralDiff :: (Double -> Double) -> Double -> Double -> Double
centralDiff f x0 h = ((f (x0 + h)) - (f (x0-h))) / (2*h)

secondDir :: (Double -> Double) -> Double -> Double -> Double
secondDir f x0 h = ((f (x0 + h)) - 2 * (f x0) + (f (x0 - h))) / (h*h)

-- Integration
rienmannSums :: (Double -> Double) -> Double -> Double -> Double -> Double
rienmannSums f a b n = dx * foldr (\l r -> (f $ a + (l * dx)) + r) 0 range
	where 
		dx = (b-a)/n
		range = [0.5..n-0.5]


lagrange :: (Double -> Double) -> Double -> [Double] -> Double
lagrange f x xs = sum $ zipWith (*) (map f xs) (map lamb xs)
	where
		lamb xi = product $ map (\xj -> (x-xj)/(xi-xj)) (delete xi xs)                                               

trapazoid :: (Double -> Double) -> Double -> Double -> Double
trapazoid f a b = (b - a) / 2 * ( (f a) + (f b))

simpsons :: (Double -> Double) -> Double -> Double -> Double
simpsons f a b = (b - a) / 2 * ((f a) + 4 * (f ((a+b) / 2 )) + (f b))

gaussQuad :: (Double -> Double) -> Double -> Double -> Double
gaussQuad f a b = (b - a) / 2 * (alpha0 * (f v0) + alpha1 * (f v1))
	where 
		z0 = -1/(sqrt 3) 
		z1 = (-z0)
		alpha0 = 1
		alpha1 = 1
		v0 = ((z0*(b-a)+a+b))/2
		v1 = ((z1*(b-a)+a+b))/2

-- adaptiveQuad f a b err 
--  	|  

-- main :: IO ()
-- main = do 
-- 	let f x = x**2 + x + 1
-- 	let g x = x**x
-- 	let err = 10**(-7) :: Double
-- 	putStrLn "\nLagrange::"
-- 	print $ g (1.0/3.0)
-- 	print $ lagrange g (1.0/3.0) ([0.00,0.01..1.00]::[Double])
-- 	putStrLn "\nDifferentiation::"
-- 	print $ forwardDiff f 0 err
-- 	print $ centralDiff f 0 err
-- 	print $ secondDir f 0 err
-- 	print $ forwardDiff g 3 err
-- 	print $ centralDiff g 3 err
-- 	putStrLn "\nIntegration::"
-- 	print $ rienmannSums g pi 4 10
-- 	print $ rienmannSums g pi 4 100
-- 	print $ rienmannSums g pi 4 1000
-- 	print $ rienmannSums g pi 4 10000
-- 	putStrLn $ "\nMore Integration::"
-- 	print $ trapazoid g pi 4
-- 	print $ simpsons g pi 4
-- 	print $ gaussQuad g pi 4







