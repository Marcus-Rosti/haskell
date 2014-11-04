import Data.List
import Data.Bits
import Data.Maybe

--My log(n) time wasn't giving the right output
--Haskell wiki for fast Fibs
--Did use bitsize but that's depri
fib :: Int -> Integer
fib n = snd . foldl' fib' (1, 0) . dropWhile not $
            [testBit n k | k <- let s = finiteBitSize n in [s-1,s-2..0]]
    where
        fib' (f, g) p
            | p         = (f*(f+2*g), ss)
            | otherwise = (ss, g*(2*f-g))
            where ss = f*f+g*g

digitArray:: Integer -> [Int]
digitArray 0 = []
digitArray x = digitArray (div x 10) ++ [mod x 10]


main = do
	let fibs = [fib x | x <- [4..10], y <- digitArray x]
	print fibs