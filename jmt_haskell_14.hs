import Data.List.Split
import Data.Bits
import Text.Printf

import Data.List
import Data.Maybe

if' True  x _ = x
if' False _ y = y

getIndices :: (a -> Bool) -> [a] -> [Int]
getIndices f x = map (snd) $ filter (f . fst) k
  where k = zip x [0,1..]

dropElements :: (Eq a) => [a] -> [a] -> [a]
dropElements x r = filter (\t -> not $ t `elem` r) x

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

doLine :: Int -> [Int] -> [(Int,Int)]
doLine n x = zip (getIndices (== 1) x) (repeat n)
  where k = zip x [0,1..]

toPairs :: Int -> [[Int]] -> [(Int,Int)]
toPairs _ [] = []
toPairs n (x:xs) = (doLine n x) ++ (toPairs (n+1) xs)

nghbr :: (Int,Int) -> (Int,Int) -> Bool
nghbr (x1,y1) (x2,y2) = ((x1 == x2) && (y1 + 1 == y2)) ||
                        ((x1 == x2) && (y1 - 1 == y2)) ||
                        ((x1 + 1 == x2) && (y1 == y2)) ||
                        ((x1 - 1 == x2) && (y1 == y2))

isNeighbour :: [(Int,Int)] -> (Int,Int) -> Bool
isNeighbour [] _ = False
isNeighbour (x:xs) t
  | nghbr x t = True
  | otherwise = isNeighbour xs t

getRegion ::  ([(Int,Int)], [(Int,Int)])-> ([(Int,Int)], [(Int,Int)])
getRegion ([], todo) = getRegion ([head todo], tail todo)
getRegion (rgn, todo)
  | length s == 0 = (rgn, todo)
  | otherwise     = getRegion (rgn', todo')
  where s = filter (isNeighbour rgn) todo
        rgn' = rgn ++ s
        todo' = dropElements todo s

getAllRegions :: [(Int,Int)] -> [[(Int,Int)]]
getAllRegions [] = []
getAllRegions t = [r] ++ getAllRegions t'
  where (r, t') = getRegion ([], t)

main = do
  let is = map (createInput "jxqlasbh") [0,1..127]
  let ts = map (createLine) is
  putStr "Part 1: "
  putStrLn . show . sum $ concat ts

  let pairs = toPairs 0 ts
  let s = getAllRegions $ pairs
  putStr "Part 2: "
  putStrLn . show $ length s


