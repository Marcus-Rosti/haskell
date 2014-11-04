import Data.List
import Data.Bits
import Data.Maybe

fact :: Int -> Int
fact n = product [1..n]

digitArray:: Int -> [Int]
digitArray 0 = []
digitArray x = digitArray (div x 10) ++ [mod x 10]

isDigFac :: Int -> Bool
isDigFac int = (==) (fromIntegral int) (sum $ map fact (digitArray int))

main = do
	print $ sum [nums | nums <- [3..1000000], isDigFac nums]