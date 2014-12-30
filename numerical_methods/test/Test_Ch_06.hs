module Test_Ch_06 (ch_06Suite_Props,ch_06Suite_Units) where

import Test.Tasty
import Test.Tasty.SmallCheck as SC
--import Test.Tasty.QuickCheck as SC
import Test.Tasty.HUnit

import Ch_06 (forwardDiff, centralDiff, secondDir, lagrange, trapazoid, rienmannSums, gaussQuad)
import Data.Maybe

ch_06Suite_Props = testGroup "Ch 6 Properties" [forwardDiff_props,centralDiff_props]
ch_06Suite_Units = testGroup "Ch 6 Units" [forwardDiff_Units]


err :: Double
err = 10**(-4)

forwardDiff_Units = testGroup "Forward Differences Unit Tests"
	[ testCase "ForwardDiff arctan 0 > 1 - err" $
		forwardDiff atan 0 err  `compare` (1-err) @?= GT,
	  testCase "ForwardDiff arctan 0 < 1 + err" $
		forwardDiff atan 0 err  `compare` (1+err) @?= LT
	]

e :: Double
e = exp 1

f x = x

atanP x = 1/(x*x+1)

forwardDiff_props = testGroup "Forward Difference Tests - Checked by SmallCheck"
	[ 
		SC.testProperty "f(x) = x should give 1 everywhere" $
			\x -> forwardDiff (\y -> y) (x::Double) err <= 1 + err,
	  	SC.testProperty "f(x) = x should give 1 everywhere" $
			\x -> forwardDiff (\y -> y) (x::Double) err >= 1 - err,
		
		SC.testProperty "f(x) = arctan x should give < 1/(x^2 +1) + err" $
			\x -> forwardDiff atan (x::Double) err <= (atanP x) + err,
		SC.testProperty "f(x) = arctan x should give > 1/(x^2 +1) - err" $
			\x -> forwardDiff atan (x::Double) err >= (atanP x) - err,
		
		SC.testProperty "f(x) = e^ (sin x +x) should give < ... + err" $
			\x -> forwardDiff sin (x::Double) err <= (cos x) + err,
		SC.testProperty "f(x) = e^ (sin x +x) should give > ... - err" $
			\x -> forwardDiff sin (x::Double) err >= (cos x) - err
	]

centralDiff_props = testGroup "Central Difference Test - Checked by SmallCheck"
	[
		SC.testProperty "f(x) = x should give 1 everywhere" $
			\x -> centralDiff (\y -> y) (x::Double) err <= 1 + err**2,
	  	SC.testProperty "f(x) = x should give 1 everywhere" $
			\x -> centralDiff (\y -> y) (x::Double) err >= 1 - err**2,
		
		SC.testProperty "f(x) = arctan x should give < 1/(x^2 +1) + err" $
			\x -> centralDiff atan (x::Double) err <= (atanP x) + err**2,
		SC.testProperty "f(x) = arctan x should give > 1/(x^2 +1) - err" $
			\x -> centralDiff atan (x::Double) err >= (atanP x) - err**2,
		
		SC.testProperty "f(x) = e^ (sin x +x) should give < ... + err" $
			\x -> centralDiff sin (x::Double) err <= (cos x) + err**2,
		SC.testProperty "f(x) = e^ (sin x +x) should give > ... - err" $
			\x -> centralDiff sin (x::Double) err >= (cos x) - err**2
	]