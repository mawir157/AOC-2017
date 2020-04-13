import Data.List

data Mode = CLEAN | WEAK | INF | FLAG deriving (Eq, Show, Ord)
type Point = (Integer, Integer)
type Virus = (Integer, Point)
type Agar  = (Mode, Point)
type StepFn = (([Agar], Virus) -> (([Agar], Virus), Mode))

if' True x _  = x
if' False _ x = x

parseInput :: Integer -> [String] -> [Agar]
parseInput n [] = []
parseInput n (s:ss) = (parseLine n s) ++ parseInput (n-1) ss

parseLine :: Integer-> String -> [Agar]
parseLine n s = zip (replicate c INF) (zip b (replicate c n)) 
  where l = length s
        k = zip s [0..(toInteger (l - 1))]
        b = map (snd) $ filter (\(x,y) -> x == '#') k
        c = length b

virusMove :: Virus -> Virus
virusMove (d, (x, y))
  | d == 0 = (d, (x, y+1))
  | d == 1 = (d, (x+1, y))
  | d == 2 = (d, (x, y-1))
  | d == 3 = (d, (x-1, y))

virusRot :: Mode -> Virus -> Virus
virusRot m (d, p) 
  | m == FLAG  = ((d + 2) `mod` 4, p)
  | m == INF   = ((d + 1) `mod` 4, p)
  | m == WEAK  = ((d) `mod` 4, p)
  | m == CLEAN = ((d - 1) `mod` 4, p)

click2 :: StepFn
click2 (live, v)
  | m == FLAG  = ((live',                v'), CLEAN)
  | m == INF   = (([(FLAG, p)] ++ live', v'), FLAG)
  | m == WEAK  = (([(INF, p)] ++ live',  v'), INF)
  | m == CLEAN = (([(WEAK, p)] ++ live', v'), WEAK)
  where p = snd v
        m = getModeAt p live
        live' = filter (\(_,x) -> x /= p) live
        v' = virusMove $ virusRot m v

click :: StepFn 
click (live, v)
  | m == INF     = ((live', v'), CLEAN)
  | m == CLEAN   = ((live' ++ [(INF, p)], v'), INF)
  where p = snd v
        m = getModeAt p live
        live' = filter (\(_,x) -> x /= p) live
        v' = virusMove $ virusRot m v

getModeAt :: Point -> [Agar] -> Mode
getModeAt p live
  | not $ elem p points = CLEAN
  | otherwise = fst . head $ dropWhile (\(_, x) -> x /= p) live
  where points = map (snd) live

run' :: StepFn -> Integer -> ([Agar], Virus) -> ([Agar], Virus)
run' f n k
  | n == 0    = k
  | otherwise = run' f (n-1) k'
  where (k', _) = f k

runCount' :: StepFn -> Integer -> (([Agar], Virus), Integer) -> (([Agar], Virus), Integer)
runCount' f n (k, c)
  | n == 0    = (k, c)
  | otherwise = runCount' f (n-1) (k', c + t) 
  where (!k', !m) = f k
        !t = if' (m == INF) 1 0

main :: IO()
main = do
  f <- readFile "input_22.txt"
  let l = lines $ f
  let s = (toInteger $ length l)

  let t = parseInput (s - 1) l
  let m = s `div` 2
  -- putStrLn $ show t
  -- putStrLn $ show m

  -- let t = [(INF,(1,1)), (INF,(-1,0))]
  -- let m = 0

  let v = (0, (m,m))

  putStr "Part 1: "
  putStrLn . show $ snd $ runCount' click 10000 ((t, v), 0)
  putStr "Part 2: "
  let ((t',v'), c) = runCount' click2 1000000 ((t, v), 0)
  putStrLn $ show c

 
