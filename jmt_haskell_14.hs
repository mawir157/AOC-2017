import Data.List.Split
import Data.Bits
import Text.Printf

hexToBinArray :: Char -> [Int]
hexToBinArray c
  | c == '0' = [0,0,0,0]
  | c == '1' = [0,0,0,1]
  | c == '2' = [0,0,1,0]
  | c == '3' = [0,0,1,1]
  | c == '4' = [0,1,0,0]
  | c == '5' = [0,1,0,1]
  | c == '6' = [0,1,1,0]
  | c == '7' = [0,1,1,1]
  | c == '8' = [1,0,0,0]
  | c == '9' = [1,0,0,1]
  | c == 'a' = [1,0,1,0]
  | c == 'b' = [1,0,1,1]
  | c == 'c' = [1,1,0,0]
  | c == 'd' = [1,1,0,1]
  | c == 'e' = [1,1,1,0]
  | c == 'f' = [1,1,1,1]

parseInput :: String -> [Int]
parseInput s = map (fromEnum) s ++ [17, 31, 73, 47, 23]

repList :: Int -> [a] -> [a]
repList n xs = take (n * length xs) $ cycle xs

rotateList :: Int -> [a] -> [a]
rotateList _ [] = []
rotateList n xs = zipWith const (drop n (cycle xs)) xs

flipFirst :: Int -> [a] -> [a]
flipFirst n xs = (reverse $ take n xs) ++ (drop n xs)

reduce :: [Int] -> [Int]
reduce xs = map (foldl1 xor) $ chunksOf 16 xs

intToHexString :: Int -> String
intToHexString n = printf "%02x" n

finalStep :: String -> [Int]
finalStep [] = []
finalStep (s:ss) = hexToBinArray s ++ finalStep ss

createInput :: String -> Int -> [Int]
createInput s n = repList 64 $ parseInput (s ++ n')
  where n' = "-" ++ show n

apply :: (Int, Int, [Int]) -> Int -> (Int, Int, [Int])
apply (skip, pos, xs) n  = (skip + 1, pos', xs')
  where y = (length xs) - pos
        xs' = rotateList y . flipFirst n $ rotateList pos xs
        pos' = (pos + n + skip) `mod` length xs

createLine :: [Int] -> [Int]
createLine xs = k
  where (_,_,t) = foldl apply (0,0,[0,1..255]) xs
        j = concat . map (intToHexString) $ reduce t
        k = finalStep j

main = do
  let is = map (createInput "jxqlasbh") [0,1..127]
  let ts = map (createLine) is
  putStr "Part 1: "
  putStrLn . show . sum $ concat ts
