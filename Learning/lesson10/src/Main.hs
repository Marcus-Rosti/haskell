-- Main.hs

module Main where

main = do
  putStrLn "What's your name?"
  name <- getLine
  putStrLn ("Hi " ++ name)
	      