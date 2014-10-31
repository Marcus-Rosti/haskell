import Data.List
import Data.Bits
import Data.Maybe

-- phi = (/) (1.0 + sqrt 5.0 ) 2.0 :: Double

-- fib:: Integer -> Integer
-- fib x = round $ phi ** fromIntegral x / sqrt 5 :: Integer

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

main = do
 	let fibs = map fib [1..]
	print $ (+1) $ fromJust $ findIndex (\fibVal -> fibVal >= 10^999) fibs
