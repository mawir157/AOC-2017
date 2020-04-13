import Data.List
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map

import Debug.Trace

data Mode = CLEAN | WEAK | INF | FLAG | ERR deriving (Eq, Show, Ord)
type Point = (Integer, Integer)
type Virus = (Integer, Point)
type Agar  = (Mode, Point)

type AgarM = Map Point Mode
type StepFn = ((AgarM, Virus) -> ((AgarM, Virus), Mode))

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
  | otherwise = error ("Mode = " ++ (show m))

modeChange :: Mode -> Maybe Mode
modeChange m
  | m == CLEAN = Just INF
  | m == INF   = Just CLEAN
  | otherwise  = Just ERR

modeChange2 :: Mode -> Maybe Mode
modeChange2 m
  | m == CLEAN = Just WEAK
  | m == WEAK  = Just INF
  | m == INF   = Just FLAG
  | m == FLAG  = Just CLEAN
  | otherwise  = Just ERR

click :: (Mode -> Maybe Mode) -> StepFn
click f (live, v)
  | not (Map.member p live) = ((Map.insert p n live, v'), n)
  | otherwise = ((Map.update f p live, v'), m')
  where p = snd v
        n = fromMaybe ERR (f CLEAN)
        m = fromMaybe CLEAN (Map.lookup p live)
        m' = fromMaybe ERR (f m)
        v' = virusMove $ virusRot m v

runCount :: (Mode -> Maybe Mode) -> Integer -> ((AgarM, Virus), Integer) -> ((AgarM, Virus), Integer)
runCount f n (k, c)
  | n == 0    = (k, c)
  | otherwise = runCount f (n - 1) (k', c + t) 
  where (!k', !m) = (click f) k
        !t = if' (m == INF) 1 0
--------------------------------------------------------------------------------
main :: IO()
main = do
  f <- readFile "input_22.txt"
  let l = lines $ f
  let s = (toInteger $ length l)

  let t = parseInput (s - 1) l
  let m = s `div` 2

  let v = (0, (m,m))

  let t_m = Map.fromList( map (\(x,y) -> (y,x)) t)
  putStr "Part 1: "
  let tm = runCount modeChange 10000 ((t_m, v), 0)
  putStrLn . show $ snd tm
  putStr "Part 2: "
  let tm = runCount modeChange2 10000000 ((t_m, v), 0)
  putStrLn . show $ snd tm
