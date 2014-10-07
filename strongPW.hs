--at least 15 characters
--uppercase letters
--lowercase letters
--numbers
import Data.List
import Data.Char
strong::String -> Bool

strong a 
	| length a <= 14 = False
	| not $ any (isUpper) a = False
	| not $ any (isLower) a = False
	| not $ any (isNumber) a = False
	| otherwise = True
 