import Data.String
import Data.List

--meh, why not
reverseInt :: Int -> String
reverseInt = reverse.show 

--just string matching... I wonder if there's a mathy way to check this
-- // I bet there's a haskelly way too
isPalindrome :: Int -> Bool
isPalindrome x
	| show x == reverseInt x = True
	| otherwise = False

--list comprehension
allPals = [ x*y | x <- [100..999], y <- [100..999], isPalindrome (x*y)] 

main = do
	print $ maximum allPals