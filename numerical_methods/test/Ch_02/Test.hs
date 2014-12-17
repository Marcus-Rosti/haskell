module Ch_02.Test (ch_02Suite) where

import Test.HUnit

ch_02Suite = []

-- test1 = TestCase (assertEqual "For digitArray basic," [1,2,3] (digitArray 123) )

-- Bisection Tests
let f x = e**x + x
let fp x = e**x + 1
let err = 10**(-15)

bisect1 = TestCase (assertEqual "equals" 1 1)



-- 	print $ fromJust $ bisection f (-1.0) 1.0 err
-- 	print $ newton_raphson f fp 1 err
-- 	print $ secant f 1 0 err
-- 	print $ fromJust $ bisection atan (-4.9) 5.1 err