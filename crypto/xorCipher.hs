import Data.Bits
import Data.Char
import Data.List
import Data.List.Split
import qualified Data.ByteString.Char8 as B

-- TO CRACK
master_string = "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"
master_key = ' '

hexToDigit = map digitToInt

digitToHex digit = (concat [['0'..'9'],['a'..'f']]) !! digit
hexByte hex = [digitToHex (shiftR hex 4)] ++ [digitToHex (hex .&. 15)]

xorDigit (x,y) = xor x y
xorBytes x y = xor x y


hexToPairDigit s = chunksOf 2 $ map digitToInt s 
combine [x,y] = shiftL x 4 + y
combineAllDigs digits = map combine digits
combineAllStrs string = map combine (hexToPairDigit string)

decrypt l k = result
  where
    decrypted = map (\ e -> xor e k) l
    result = map chr decrypted

-- fixedCharXOR str char = result 
-- 	where 
-- 		nibbles = map xorDigit (zip (replicate (length str) (combine.hexToPairDigit) char)) (hexToPairDigit str))
-- 		hex = nibbles --map digitToHex nibbles
-- 		result =  hex

main = do
	print $ decrypt (combineAllStrs master_string) (ord master_key)