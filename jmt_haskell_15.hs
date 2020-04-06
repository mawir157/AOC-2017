base16 = 65536

modMult :: Integer -> Integer -> Integer -> Integer
modMult p i a = (i * a) `mod` p

getPowers :: Integer -> (Integer, Integer) -> [Integer]
getPowers p (i, a) = map (\x -> x `mod` base16) . scanl (modMult p) i $ repeat a

filtRedPowers :: Integer -> Integer -> (Integer, Integer) -> [Integer]
filtRedPowers b p (i, a) = map (\x -> x `mod` base16) t
  where t = filter (\x -> x `mod` b == 0) $ getPowers p (i, a)

main = do 
  let i1 = (116, 16807) 
  let i2 = (299, 48271)
  let p  = 2147483647

  putStr "Part 1: "
  let j1 = take 40000000 $ getPowers p i1
  let j2 = take 40000000 $ getPowers p i2
  let r  = filter (\(x, y) -> x == y) $ zip j1 j2
  putStrLn . show $ length r

  putStr "Part 2: "
  let k1 = take 5000000 $ filtRedPowers 4 p i1
  let k2 = take 5000000 $ filtRedPowers 8 p i2
  let t = filter (\(x, y) -> x == y) $ zip k1 k2
  putStrLn . show $ length t
