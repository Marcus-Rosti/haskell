rev [] = []
rev a = last a : rev (init a)