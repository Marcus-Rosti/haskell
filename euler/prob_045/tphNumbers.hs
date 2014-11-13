isPentagonal :: Int -> Bool
isPentagonal num = num == 

main = do
	let nums = [(x,y) | x <- [1..], y<-[1..], x*(x+1)/2 == y*(3*y-1)/2]
	print $ nums