import Data.Char
import Data.List
import Data.List.Split
import Data.Bits
import Data.Word8
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import Data.ByteString.Base64 (decode)
import Data.Either
import Control.Concurrent

-- Boilerplate 
hexToPairDigit s = chunksOf 2 $ map digitToInt s
combine [x,y] = shiftL x 4 + y
combineAllDigs digits = map combine digits
combineAllStrs string = map combine (hexToPairDigit string)
	
digitToHex digit = (concat [['0'..'9'],['a'..'f']]) !! digit
hexByte hex = [digitToHex (shiftR hex 4)] ++ [digitToHex (hex .&. 15)]

toBytes char = map ord char

buildXORKey key = (toBytes.concat.repeat) key

xorDigit (x,y) = xor x y

-- /Boilerplate

sumTheBits :: Word8 -> Int
sumTheBits x 
	| x == 0 = 0
	| otherwise = 1 + (sumTheBits (x .&. (x-1)))

sumAllTheBits :: [Word8] -> Int
sumAllTheBits x = sum (map sumTheBits x)

hammingDist :: [Word8] -> [Word8] -> Int
hammingDist x y = sumAllTheBits (map xorDigit (zip x y))


hammingDistPair :: [[[Word8]]] -> [Int]
hammingDistPair pairs = map (\ tuple -> hammingDist (head tuple) (last tuple)) pairs

chunkSizePair :: Int -> [Word8] -> [[[Word8]]]
chunkSizePair size line = chunksOf 2 $ chunksOf size line

normHammingDist :: [Word8] -> Int -> Int
normHammingDist input keySize = result
	where 
		bytePairs = chunksOf 2 $ chunksOf keySize input
		seperations = hammingDistPair bytePairs
		summation = sum seperations 
		result = quot (summation) (length bytePairs)


doSomethingElse :: C8.ByteString -> IO()
doSomethingElse withThis = print result
	where
		bytes = B.unpack withThis
		dist = map (\ size -> normHammingDist bytes size) [2..40]
		zipKeys = zip dist [2..40]
		sortedKeys = sortBy (\ (a1,b1) (a2,b2) -> compare (-1*a1) (-1*b1)) zipKeys
		top20 = map snd sortedKeys
 		result = top20

main :: IO()
main = do
	everyLine <- B.readFile "6.txt"
	let base64Lines = C8.filter (\ c -> not(c == '\n')) everyLine
	( either putStrLn doSomethingElse (decode base64Lines) )
	-- normHammingDist (decode (rights base64Lines)) 2





