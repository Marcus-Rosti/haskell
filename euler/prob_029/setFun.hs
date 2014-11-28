foldDups = foldl (\seen x -> if x `elem` seen 
									then seen 
									else seen ++ [x]) []

main = do  
	print $ length $ foldDups [a^b | a <-[2..100], b<-[2..100]]