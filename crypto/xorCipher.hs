import Data.Bits
import Data.Char
import Data.List
import Data.List.Split
import qualified Data.ByteString.Char8 as B



-- TO CRACK
master_string = "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"
master_key = 'G'

freqDict = ["the", "and", "for", "are", "but", "not", "you", "all", "any", "can", "had", "her", "was", "one", "our", "out", "day", "get", "has", "him", "his", "how", "man", "new", "now", "old", "see", "two", "way", "who", "boy", "did", "its", "let", "put", "say", "she", "too", "use"]

hexToDigit = map digitToInt

digitToHex digit = (concat [['0'..'9'],['a'..'f']]) !! digit
hexByte hex = [digitToHex (shiftR hex 4)] ++ [digitToHex (hex .&. 15)]

xorDigit (x,y) = xor x y
xorBytes x y = xor x y

hexToPairDigit s = chunksOf 2 $ map digitToInt s 
combine [x,y] = shiftL x 4 + y
combineAllDigs digits = map combine digits
combineAllStrs string = map combine (hexToPairDigit string)

unionList:: [Bool] -> Bool
unionList [] = False
unionList (x:[]) = x
unionList (x:xs) = x || (unionList xs)

comoIngles:: [Char] -> Bool
comoIngles charArray = unionList [isInfixOf key charArray | key <- freqDict]

decrypt l k = result
  where
    decrypted = map (\ e -> xor e k) l
    result = map chr decrypted

main = do
	let decrypted = decrypt (combineAllStrs master_string) (ord master_key)
	print $ comoIngles decrypted







