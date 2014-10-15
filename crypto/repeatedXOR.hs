import Data.Char
import Data.List
import Data.List.Split
import Data.Bits

hexToPairDigit s = chunksOf 2 $ map digitToInt s
combine [x,y] = shiftL x 4 + y
combineAllDigs digits = map combine digits
combineAllStrs string = map combine (hexToPairDigit string)

digitToHex digit = (concat [['0'..'9'],['a'..'f']]) !! digit
hexByte hex = [digitToHex (shiftR hex 4)] ++ [digitToHex (hex .&. 15)]

toBytes char = map ord char

buildXORKey key = (toBytes.concat.repeat) key

xorDigit (x,y) = xor x y

repeatedXOREncryption string key =concat $ map hexByte (map xorDigit (zip (toBytes string) (buildXORKey key)))


main = do
	passPhrase <- readFile "passPhrase.txt"
	print $ repeatedXOREncryption passPhrase "ICE"
