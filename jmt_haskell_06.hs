import Data.List
import Data.List.Split

parseInput x = map (read) (splitOn "\t" x) :: [Integer]

if' True  x _ = x
if' False _ y = y

redist x = zipWith (+) x' $ zipWith (+) base extra'
  where k = toInteger $ length x
        m =  maximum x
        max_i = head . sort $ elemIndices m x
        x' = take (max_i) x ++ [0] ++ drop (max_i + 1) x
        base = replicate (length x) (m `div` k)
        diff = k - (m `mod` k)
        extra = map (\x -> (toInteger max_i - x) `mod` k) [0,1..(k-1)]
        extra' = map (\x -> if' (x < diff) 0 1) extra

iterateUntilRep :: Eq a => (a -> a) -> (a, [a]) -> (a, [a])
iterateUntilRep f (x, seen)
  | x `elem` seen = (x, seen)
  | otherwise     = iterateUntilRep f (f x, seen ++ [x])

main = do 
  f <- readFile "jmt_input_06.txt"
  let t = parseInput . head $ lines f
  putStr "Part 1: "
  let r = iterateUntilRep redist (t, [])
  putStrLn . show . length $ snd r

  putStr "Part 2: "
  let r' = iterateUntilRep redist (last $ snd r, [])
  putStrLn . show . length $ snd r'
