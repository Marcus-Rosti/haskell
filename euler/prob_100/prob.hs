binnetsWork :: (Integer, Integer) -> Int -> (Integer, Integer)
binnetsWork (b,n) 0 = (b,n)
binnetsWork (b,n) 1 = (3 * b + 2 * n - 2,4 * b + 3 * n - 3)
binnetsWork (b,n) i = binnetsWork (3 * b + 2 * n - 2,4 * b + 3 * n - 3) (i-1)

binnets :: Int -> (Integer,Integer)
binnets 0 = (15,21)
binnets i = binnetsWork (3 * 15 + 2 * 21 - 2, 4 * 15 + 3 * 21 - 3) (i-1)


main = do
  print $ take 1 $ dropWhile (\(f,s) -> s <10^12) $ map binnets [0..]
