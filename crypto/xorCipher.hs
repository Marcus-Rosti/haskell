import Data.Bits
import Data.Char
import Data.List
import Data.List.Split

-- TO CRACK
-- 1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736

hexToDigit = map digitToInt

digitToHex digit = (concat [['0'..'9'],['a'..'f']]) !! digit

xorDigit (x,y) = xor x y

hexToPairDigit s = chunksOf 2 $ map digitToInt s 
combine [x,y] = shiftL x 4 + y

combineAll digits = map combine digits
combineAll text = map combine (hexToPairDigit text) 



fixedXOR x y = result 
	where
		nibbles = map xorDigit (zip (hexToDigit x) (hexToDigit y))
		hex = map digitToHex nibbles
		result =  hex

-- fixedCharXOR str char = result 
-- 	where 
-- 		nibbles = foldl xorDigit char (hexToDigit str) 
-- 		hex = map digitToHex nibbles
-- 		result =  hex

main = do
	print "hello"