module Test_Ch_02 (ch_02Suite_Props,ch_02Suite_Units) where

import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Ch_02 (bisection, newton_raphson, secant, horner)
import Data.Maybe

{- 
Need to test ::
	bisection, newton_raphson,secant, horner 
-}

{-

tests :: TestTree
tests = testGroup "Tests" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties" [scProps, qcProps]

scProps = testGroup "(checked by SmallCheck)"
  [ SC.testProperty "sort == sort . reverse" $
      \list -> sort (list :: [Int]) == sort (reverse list)
  , SC.testProperty "Fermat's little theorem" $
      \x -> ((x :: Integer)^7 - x) `mod` 7 == 0
  -- the following property does not hold
  , SC.testProperty "Fermat's last theorem" $
      \x y z n ->
        (n :: Integer) >= 3 SC.==> x^n + y^n /= (z^n :: Integer)
  ]

qcProps = testGroup "(checked by QuickCheck)"
  [ QC.testProperty "sort == sort . reverse" $
      \list -> sort (list :: [Int]) == sort (reverse list)
  , QC.testProperty "Fermat's little theorem" $
      \x -> ((x :: Integer)^7 - x) `mod` 7 == 0
  -- the following property does not hold
  , QC.testProperty "Fermat's last theorem" $
      \x y z n ->
        (n :: Integer) >= 3 QC.==> x^n + y^n /= (z^n :: Integer)
  ]

unitTests = testGroup "Unit tests"
  [ testCase "List comparison (different length)" $
      [1, 2, 3] `compare` [1,2] @?= GT

  -- the following test does not hold
  , testCase "List comparison (same length)" $
      [1, 2, 3] `compare` [1,2,2] @?= LT
  ]

-}

e :: Double
e = 2.7182818284590452353602874713527

f x = e**x + x
fp x = e**x + 1
fRoot = (-0.56714329040978387299996866221035554975381578718651250813513)



err = 10**(-10)

ch_02Suite_Props = testGroup "Properties" [newton_raphson_props]
ch_02Suite_Units = bisectionUnits

bisectionUnits = testGroup "Bisection Unit Tests"
	[ testCase "Bisection arctan [-4.9,5.1] > err" $
		(fromJust $ bisection atan ((-4.9),5.1) err ) `compare` (-err) @?= GT,

    testCase "Bisection arctan [-4.9,5.1] < err" $
    (fromJust $ bisection atan ((-4.9),5.1) err ) `compare` (err) @?= LT,

    testCase "Bisection arctan [1,5.1] == Nothing" $
    bisection atan (1,5.1) err @?= Nothing,

    testCase " Bisection e^x + x [-4.9,5.1] > err" $
      (fromJust $ bisection f ((-4.9),5.1) err ) `compare` (fRoot-err) @?= GT,

    testCase " Bisection e^x + x [-4.9,5.1] < err" $
      (fromJust $ bisection f ((-4.9),5.1) err ) `compare` (fRoot+err) @?= LT,

    testCase " Bisection e^x + x [0,5.1] ==nothing" $
      bisection f (0,5.1) err @?= Nothing
	]



newton_raphson_props = testGroup "Newton-Raphson Propeties\nChecked by SmallCheck"
  [ SC.testProperty "e^x + x should converge from anywhere" $
      \x -> (fromJust $ newton_raphson f fp (x :: Double) err) <= fRoot+err,
    SC.testProperty "e^x + x should converge from anywhere" $
      \x -> (fromJust $ newton_raphson f fp (x :: Double) err) >= fRoot-err

  ]  

-- 	print $ fromJust $ bisection f (-1.0) 1.0 err
-- 	print $ newton_raphson f fp 1 err
-- 	print $ secant f 1 0 err
-- 	print $ fromJust $ bisection atan (-4.9) 5.1 err