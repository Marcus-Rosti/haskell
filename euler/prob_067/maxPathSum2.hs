parse:: String -> [[Int]]
parse = map (map read . words) . lines 

pathSolve :: [[Int]] -> Int
pathSolve = head . foldr1 findtreePath


{-
Follow the stars from left to right

Build the tree from the bottom
						3
				6(15)*
		4(19)			9*
3(23)*			4(13)*
		7(20)*			5	
				2(10)
						8*
-}
findtreePath:: [Int] -> [Int] -> [Int]
findtreePath [] [topOfTheTree] = [topOfTheTree]
findtreePath (upper:restOfTop) (left:right:theRestofBottom) = upper + max left right : findtreePath restOfTop (right:theRestofBottom)


main = do
	myFile <- readFile "triangle.txt"
	let desLines = parse myFile
	print $ pathSolve desLines
