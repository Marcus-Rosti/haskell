--even devisible? more like the largest common multiple of all of them

main = do
 	print $ foldr lcm 1 [2..20]