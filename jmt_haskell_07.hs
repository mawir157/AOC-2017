import Data.List
import Data.List.Split

if' True  x _ = x
if' False _ y = y

type Node = (String, Integer, [String])

fst' (x,_,_) = x
snd' (_,y,_) = y
thd' (_,_,z) = z

countElem :: Eq a => [a] -> a -> Int
countElem x n = length $ filter (\t -> t == n) x

parseInput :: String -> (String, Integer, [String])
parseInput s = (s', n', ss')
  where s' = takeWhile (\x -> x /= ' ') s
        n = takeWhile (\x -> x /= ')') . drop 1 $ dropWhile (\x -> x /= '(') s
        n' = read n :: Integer
        ss = drop 2 $ dropWhile (\x -> x /= '>') s
        ss' = splitOn ", " ss

findParent :: [Node] -> Node -> Node
findParent [] k = k
findParent (x:xs) (s, n, c)
  | s `elem` (thd' x) = x
  | otherwise        = findParent xs (s, n, c)

findChildren :: [Node] -> Node -> [Node]
findChildren [] s = []
findChildren (x:xs) (s, n, c) = (if' ((fst' x) `elem` c) [x] []) ++  findChildren xs (s, n, c)

getWeights :: [Node] -> Node -> Integer
getWeights f (s, n, c) = n + sum (map (getWeights f) childs)
  where childs = findChildren f (s, n, c)

wrongWeight :: [Node] -> Node -> Node
wrongWeight f n
  | length childs > 0 = bad
  | otherwise         = n
  where childs = findChildren f n
        ws = map (getWeights f) childs
        wsct = map (countElem ws) ws
        bad = fst . head . filter (\x -> snd x == 1) $ zip childs wsct

badWeight :: [Node] -> Node -> Node
badWeight f n
  | a == n    = n
  | otherwise = badWeight f a
  where a = wrongWeight f n

findRoot :: [Node] -> Node -> Node
findRoot x p
  | p == p'   = p
  | otherwise = findRoot x p'
  where p' = findParent x p

findBadTree :: [Node] -> Node -> [[Node]]
findBadTree t k
  | (length $ nub s) == 1 = []
  | otherwise             = [a] ++ findBadTree t k'
  where a = findChildren t k
        s = map (getWeights t) a
        k' = wrongWeight t k

main = do 
  f <- readFile "input_07.txt"
  let t = map (parseInput) $ lines f
  putStr "Part 1: "
  let k = findRoot t (head t)
  putStrLn . show $ fst' k

  putStr "Part 2: "
  let test = findBadTree t k
  let tt = last test
  let ts = map (getWeights t) tt
  let ts' = map (\x-> x - minimum ts) ts
  let tq = map (snd') tt
  let r = zip ts' tq
  let p = head $ dropWhile (\x -> fst x == 0) r
  putStrLn $ show ((snd p) - (fst p))
