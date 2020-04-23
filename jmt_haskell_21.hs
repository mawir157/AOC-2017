import Data.List.Split

type Grid = [[Char]]
type Rule = (Grid, Grid)

intSquareRoot :: Int -> Int
intSquareRoot n = try 0 where
  try i   | i*i <= n    = try (i + 1) 
          | True        = i - 1

parseLine :: String -> Rule
parseLine s = (splitOn "/" lhs, splitOn "/" rhs)
  where [lhs, rhs] = splitOn " => " s

splitGrid :: Grid -> [Grid]
splitGrid grid
  | gridEven  = split2 grid
  | otherwise = split3 grid
  where gridEven = mod (length grid) 2 == 0

split2 :: Grid -> [Grid]
split2 [] = []
split2 (x:y:xs) = p ++ (split2 xs)
  where x2 = chunksOf 2 x
        y2 = chunksOf 2 y
        p = zipWith (\a1 a2 -> [a1, a2]) x2 y2

split3 :: Grid -> [Grid]
split3 [] = []
split3 (x:y:z:xs) = p ++ (split3 xs)
  where x3 = chunksOf 3 x
        y3 = chunksOf 3 y
        z3 = chunksOf 3 z
        p = zipWith3 (\a1 a2 a3 -> [a1, a2, a3]) x3 y3 z3

matchGrid :: Grid -> Grid -> Bool
matchGrid g1 g2 = f (length g1) g1 g2
  where f n g1 g2 = (elem g2 $ allConfigs n g1)

ref :: Grid -> Grid
ref gs = map (reverse) gs

rot2 :: Grid -> Grid
rot2 [[a, b],[c, d]] = [[b, d],[a, c]]

rot3 :: Grid -> Grid
rot3 [[a,b,c],[d,e,f],[g,h,j]] = [[c,f,j],[b,e,h],[a,d,g]]

allConfigs :: Int -> Grid -> [Grid]
allConfigs n g
  | n == 2 = g2 ++ map (ref) g2
  | n == 3 = g3 ++ map (ref) g3
  where g2 = [g, rot2 g, rot2$rot2 g, rot2.rot2$rot2 g]
        g3 = [g, rot3 g, rot3$rot3 g, rot3.rot3$rot3 g]

applyRule :: [Rule] -> Grid -> Grid
applyRule [] g = error (show g)
applyRule (r:rs) g 
  | matchGrid (fst r) g = snd r
  | otherwise           = applyRule rs g

recombineGrid :: [Grid] -> Grid
recombineGrid gs = recombineGrid' (intSquareRoot $ length gs) gs

recombineGrid' :: Int -> [Grid] -> Grid
recombineGrid' n [] = []
recombineGrid' n gs = p ++ recombineGrid' n (drop n gs)
  where block = length (gs!!0)
        p = map (catNthRows (take n gs)) [0..(block-1)]

catNthRows :: [[[a]]] -> Int ->  [a]
catNthRows gs n = concat $ map (\x -> x!!n) gs

tick :: [Rule] -> Grid -> Grid
tick r g = recombineGrid . map (applyRule r) $ splitGrid g

tickN :: Int -> [Rule] -> Grid -> Grid
tickN 0 _ g = g
tickN n r g = tickN (n-1) r (tick r g)

main = do
  f <- readFile "input_21.txt"
  let rules = map (parseLine) $ lines f
  let initState = [".#.","..#","###"]

  putStr "Part 1: "
  let w = tickN 5 rules initState
  putStrLn $ show . length . filter (\x -> x == '#') $ concat w

  putStr "Part 2: "
  let v = tickN 18 rules initState
  putStrLn $ show . length . filter (\x -> x == '#') $ concat v
