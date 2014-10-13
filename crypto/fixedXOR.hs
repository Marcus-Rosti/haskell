import Data.Bits
import Data.Char
import Data.List
import Data.List.Split


-- 1c0111001f010100061a024b53535009181c
-- 686974207468652062756c6c277320657965

hexToDigit = map digitToInt

digitToHex digit = (concat [['0'..'9'],['a'..'f']]) !! digit

xorDigit (x,y) = xor x y

fixedXOR x y = result 
	where
		nibbles =map xorDigit (zip (hexToDigit x) (hexToDigit y))
		hex = map digitToHex nibbles
		result =  hex

main = do
	someBits <- getLine
	key <- getLine
	print $ fixedXOR someBits key