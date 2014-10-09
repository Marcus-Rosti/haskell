import Data.Bits
import Data.Char
import Data.List
import Data.List.Split
import System.IO


indexB64 i = b64numerals !! i
	where b64numerals = concat [['A'..'Z'],['a'..'z'],['0'..'9'],['+'],['/']]

checkifHex = all isHexDigit

-- hexToBase64 hex = base_64

bitShiftHexPairs (x:[]) = bitShiftHexPairs (x:0:0:[])
bitShiftHexPairs (x:y:[]) = bitShiftHexPairs (x:y:0:[])
bitShiftHexPairs (x:y:z:[]) = (a,b,c,d)
	where 
		a = shiftR x 2
		b = shiftL x 4 .&. 48 + shiftR y 4
		c = shiftL y 4 .&. 15 + shiftR z 6
		d = z .&. 63

hexToPairDigit s = chunksOf 2 $ map digitToInt s 

combine [x,y] = shiftL x 4 + y
 
combineAll digits = map combine digits

tuple2base64 (a,b,c,d) = (indexB64 a) : (indexB64 b) : (indexB64 c) : [indexB64 d]

mod3 val = mod val 3

hexToBase64 hex = base64
	where
		digitPairs = hexToPairDigit hex
		hexPairs = combineAll digitPairs
		tuples = map bitShiftHexPairs (chunksOf 3 hexPairs)
		noPadding = concat (map tuple2base64 tuples)
		pad = (mod3.(3-).mod3.length) hexPairs
		wPadding = reverse.(drop pad).reverse $ noPadding
		base64 = wPadding ++ concat ["=" | r <- [1..pad]]

		


main = do
	hex <- getLine
	putStr $ concat [(hexToBase64 hex),"\n"]



