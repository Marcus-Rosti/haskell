-- Horribly inef but it works!
pythagNumbs = [ (a,b,c,a* b*c) | a <- [1..500], b <-[a..500], c <- [b..500], a^2+b^2 == c^2, a+b+c==1000]

main = do
	print $ pythagNumbs