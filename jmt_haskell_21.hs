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
matchGrid g1 g2
  | length g1 == 2 = matchGrid2 g1 g2
  | otherwise      = matchGrid3 g1 g2

matchGrid3 :: Grid -> Grid -> Bool
matchGrid3 [[a,b,c],[d,e,f],[g,h,j]] g2
  | [[a,b,c],[d,e,f],[g,h,j]] == g2 = True
  | [[c,f,j],[b,e,h],[a,d,g]] == g2 = True
  | [[j,h,g],[f,e,d],[c,b,a]] == g2 = True
  | [[g,d,a],[h,e,b],[j,f,c]] == g2 = True
  | [[a,d,g],[b,e,h],[c,f,j]] == g2 = True
  | [[g,h,j],[d,e,f],[a,b,c]] == g2 = True
  | [[j,f,c],[h,e,b],[g,d,a]] == g2 = True
  | [[c,b,a],[f,e,d],[j,h,g]] == g2 = True
  | otherwise                       = False

matchGrid2 :: Grid -> Grid -> Bool
matchGrid2 [[a, b],[c, d]] g2
  | [[a, b],[c, d]] == g2 = True
  | [[b, d],[a, c]] == g2 = True
  | [[c, a],[d, b]] == g2 = True
  | [[d, c],[b, a]] == g2 = True
  | [[a, c],[b, d]] == g2 = True
  | [[b, a],[d, c]] == g2 = True
  | [[c, d],[a, b]] == g2 = True
  | [[d, b],[c, a]] == g2 = True
  | otherwise             = False

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
