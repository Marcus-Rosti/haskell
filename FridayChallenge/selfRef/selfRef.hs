-- | Main entry point to the application.
module Main where

zeroTo9 :: [Int]
zeroTo9 = [0..9]

toDigs :: Integral x => x -> [x]
toDigs 0 = []
toDigs x = toDigs (x `div` 10) ++ [x `mod` 10]

numbOfXinY :: Int -> [Int] -> Int
numbOfXinY x y = length $ filter (==x) y

selfDescribing :: Int -> Bool
selfDescribing x = result
    where
        digits = toDigs x
        count = map (`numbOfXinY` digits) [0..9]
        result = count == digits



-- | The main entry point.
main :: IO ()
main = do
	let allPossible = [1000000000..9999999999]
	--let allPossible = [1000..9999]
	--print $ length allPossible
	print $ filter selfDescribing allPossible
