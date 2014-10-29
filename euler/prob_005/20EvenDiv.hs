--even devisible? more like the largest common multiple of all of them

--foldr1 doesn't need a starting value
--foldr(THE NUMBER ONE not an l...)
main = do
 	print $ foldr1 lcm [1..20]