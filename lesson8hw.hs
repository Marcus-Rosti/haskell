module Main where

import System.IO
import Data.List

main = do 
	putStrLn "reading a file... FORWARDS"
	myStr <- readFile "input.txt"
	putStrLn "writing to file... SDRAWKCAB"
	writeFile "output.txt" (reverse myStr)
