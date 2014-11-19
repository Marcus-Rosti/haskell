import Control.Parallel
import Control.Parallel.Strategies

triangleNumbs:: Int -> Int -> Int -> Bool
triangleNumbs a b c = b + c > a && a + c > b && a + b > c


intRightTri n = length $ [(a,b,c) |a<-[1..n `div` 2], b<-[1..n `div` 2], c<-[1..n `div` 2], a+b+c == n, triangleNumbs a b c]

main = do
	--print $ parMap rpar intRightTri [700..1000]
	print $ intRightTri 