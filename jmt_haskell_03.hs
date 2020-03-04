floorOddSqrt n = last $ takeWhile (\x -> x*x <= n) [1,3..]

fn n = p
  where k = (floorOddSqrt n)
        k' = (k - 1) `div` 2
        m = n - k * k
        m' = (m - 1) `div` (k + 1)
        t = (m - 1) `mod` (k + 1)
        p
          | m  == 0 = (k', k')
          | m' == 0 = (k' + 1, k' - t)
          | m' == 1 = (k' - t, k' - k)
          | m' == 2 = (k' - k, k' - k + t + 1)
          | m' == 3 = (k' - k + 1 + t, k' + 1)

manDist (a,b) = (abs a) + (abs b)

main = do 
  putStr "Part 1: "
  putStrLn . show . manDist $ fn 277678
  putStr "Part 2: "
  putStrLn " "

