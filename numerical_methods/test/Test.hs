module Main where

import Test.Framework (defaultMain)

import Ch_02.Test
import Ch_03.Test
import Ch_04.Test

main :: IO ()
main = defaultMain [ch_02Suite,ch_03Suite,ch_06Suite]