import Data.Char
import Data.List
import Data.List.Split
import Data.Bits
import Data.Word8
import Data.Maybe
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import Data.ByteString.Base64 (decode)
import Data.Either
import Control.Concurrent

-- Boilerplate 
freqDict = ["th", "he", "an", "in", "er", "on", "re", "ed", "nd", "ha", "at", "en", "es", "of", "nt", "ea", "ti", "to", "io", "le", "is", "ou", "ar", "as", "de", "rt", "ve","the", "be", "to", "of", "and", "a", "in", "that", "have", "i","it", "for", "not", "on", "with", "he", "as", "you", "do", "at","this", "but", "his", "by", "from", "they", "we", "say", "her","she", "or", "an", "will", "my", "one", "all", "would", "there","their", "what", "so", "up", "out", "if", "about", "who", "get","which", "go", "me", "when", "make", "can", "like", "time", "no","just", "him", "know", "take", "people", "into", "year", "your","good", "some", "could", "them", "see", "other", "than", "then","now", "look", "only", "come", "its", "over", "think", "also","back", "after", "use", "two", "how", "our", "work", "first","well", "way", "even", "new", "want", "because", "any", "these","give", "day", "most", "us"]

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

scoreList::[Bool] -> Int
scoreList [] = 0
scoreList (x:xs) 
	| x == True = 1 + scoreList xs
	| x == False = scoreList xs

tryAllByteKeys string = [score (decrypt string key) | key <- [0,1..127]]

comoIngles:: [Char] -> Bool
comoIngles charArray = unionList [isInfixOf key charArray | key <- freqDict]

score:: [Char] -> Int
score charArray = scoreList [isInfixOf key charArray | key <- freqDict]


-- mostLikelyKey :: [Char] -> Char
mostLikelyKey string = fromJust (elemIndex (maximum (tryAllByteKeys string)) (tryAllByteKeys string) ) 

findDecryption hex = decrypt (combineAllStrs hex) (mostLikelyKey (combineAllStrs hex))


decrypt string key = result
  where
    decrypted = map (\ e -> xor e key) string
    result = map chr decrypted


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

decryptKeySizes bytes size = result
	where
		chunks = chunksOf size bytes
		result = map chr chunks

findKeySize :: C8.ByteString -> IO()
findKeySize withThis = print result
	where
		bytes = B.unpack withThis
		dist = map (\ size -> normHammingDist bytes size) [2..40]
		zipKeys = zip dist [2..5]
		sortedKeys = sortBy (\ (a1,b1) (a2,b2) -> compare (-1*a1) (-1*b1)) zipKeys
		top20 = map snd sortedKeys
 		result = map (\ size -> decryptKeySizes bytes size) top20

main :: IO()
main = do
	everyLine <- B.readFile "6.txt"
	let base64Lines = C8.filter (\ c -> not(c == '\n')) everyLine
	( either putStrLn findKeySize (decode base64Lines) )
	-- normHammingDist (decode (rights base64Lines)) 2





