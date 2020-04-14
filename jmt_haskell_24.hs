import Data.List
import Data.Tuple
import Data.List.Split

type Comp = (Integer, Integer)
type Bridge = [Comp]

type BdgState = (Bridge, [Comp])

parseLine :: String -> Comp
parseLine s = (read (ss!!0) :: Integer, read (ss!!1) :: Integer)
  where ss = splitOn "/" s

extendBridge :: Bridge -> Comp -> Bridge
extendBridge b p = b ++ [p]

removeLR :: [Comp] -> Comp -> [Comp]
removeLR ps (l,r) =
  filter (\x -> (x /= (r,l)) && (x /= (l,r))) ps

build' :: BdgState -> [BdgState]
build' (b, ps)
  | length gs > 0 = concat $ map (build') bs'
  | otherwise     = [(b, ps)]
  where end = snd $ last b
        gsL = filter (\(x,_) -> x == end) ps
        gsR = map (swap) $ filter (\(_,x) -> x == end) ps
        gs = nub (gsL ++ gsR)
        ps' = map (\x -> removeLR ps x) gs
        b' = map (\x -> extendBridge b x) gs
        bs' = zip b' ps'

getStrength :: BdgState -> Integer
getStrength (b, _) = sum $ map (\(x,y) -> x + y) b

bestPart2 :: [BdgState] -> BdgState
bestPart2 [x] = x
bestPart2 (x:y:xs)
  | (length $ fst x) > (length $ fst y) = bestPart2 (x:xs)
  | (length $ fst x) < (length $ fst y) = bestPart2 (y:xs)
  | (getStrength x) > (getStrength y)   = bestPart2 (x:xs)
  | otherwise                           = bestPart2 (y:xs)

main :: IO()
main = do
  f <- readFile "input_24.txt"
  let l = lines $ f
  let s = map (parseLine) l

  let t = build' ([(0,0)], s)
  putStr "Part 1: "
  putStrLn . show . maximum $ map (getStrength) t
  putStr "Part 2: "
  putStrLn . show . getStrength $ bestPart2 t
