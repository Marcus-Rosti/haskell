
import Test.Tasty
import Test.Tasty.SmallCheck as SC
--import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Test_Ch_02 (ch_02Suite_Props,ch_02Suite_Units)
main = defaultMain tests


tests :: TestTree
tests = testGroup "Tests" [properties,unitTests]

unitTests = ch_02Suite_Units
properties = ch_02Suite_Props
