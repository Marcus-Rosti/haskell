-- All hexagonals are triangular
hexagonals = [n*(2*n-1) | n<-[1..]]

isPentagonal :: Integer -> Bool
isPentagonal x = n == (fromIntegral.floor) n
	where
		n = ((1/2) + sqrt (1/4 + 6*fromIntegral x)) / 3

main = do
	-- print $ filter (isPentagonal) $ filter (isHexagonal) triangulars
	print $ take 3 $ filter isPentagonal hexagonals