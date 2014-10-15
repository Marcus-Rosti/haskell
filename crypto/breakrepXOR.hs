import Data.Char
import Data.List
import Data.List.Split
import Data.Bits

--Boilerplate
hexToPairDigit s = chunksOf 2 $ map digitToInt s
combine [x,y] = shiftL x 4 + y
combineAllDigs digits = map combine digits
combineAllStrs string = map combine (hexToPairDigit string)

digitToHex digit = (concat [['0'..'9'],['a'..'f']]) !! digit
hexByte hex = [digitToHex (shiftR hex 4)] ++ [digitToHex (hex .&. 15)]

toBytes char = map ord char

buildXORKey key = (toBytes.concat.repeat) key

xorDigit (x,y) = xor x y
--/Boilerplate

sumTheBits :: Int -> Int
sumTheBits x 
	| x == 0 = 0
	| otherwise = 1 + (sumTheBits (x .&. (x-1)))

sumAllTheBits :: [Int] -> Int
sumAllTheBits x = sum (map sumTheBits x)

hammingDist :: [Int] -> [Int] -> Int
hammingDist x y 
	| (length x) == (length y) = sumAllTheBits (map xorDigit (zip x y))
	| otherwise = error "Unequal, probably"


main = do
	let first = map ord "this is a test"
	let second = map ord "wokka wokka!!!"
	print $ hammingDist first second