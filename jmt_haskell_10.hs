import Data.List.Split
import Data.Bits
import Text.Printf

parseInput :: String -> [Int]
parseInput s = map (read) (splitOn "," s) :: [Int]

parseInput2 :: String -> [Int]
parseInput2 s = map (fromEnum) s ++ [17, 31, 73, 47, 23]

reduce :: [Int] -> [Int]
reduce xs = map (foldl1 xor) $ chunksOf 16 xs

repList :: Int -> [a] -> [a]
repList n xs = take (n * length xs) $ cycle xs

rotateList :: Int -> [a] -> [a]
rotateList _ [] = []
rotateList n xs = zipWith const (drop n (cycle xs)) xs

flipFirst :: Int -> [a] -> [a]
flipFirst n xs = (reverse $ take n xs) ++ (drop n xs)

apply :: (Int, Int, [Int]) -> Int -> (Int, Int, [Int])
apply (skip, pos, xs) n  = (skip + 1, pos', xs')
  where y = (length xs) - pos
        xs' = rotateList y . flipFirst n $ rotateList pos xs
        pos' = (pos + n + skip) `mod` length xs

intToHexString :: Int -> String
intToHexString n = printf "%02x" n

main = do 
  f <- readFile "input_10.txt"
  let i = parseInput f
  let (_,_,k) = foldl apply (0,0,[0,1..255]) i
  putStr "Part 1: "
  putStrLn . show . product $ take 2 k
  putStr "Part 2: "
  let i2 = parseInput2 . head $ lines f
  let (_,_,k') = foldl apply (0,0,[0,1..255]) $ repList 64 i2
  let j = concat . map (intToHexString) $ reduce k'
  putStrLn $ show j
