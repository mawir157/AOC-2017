import Data.List
import Data.Maybe

data Dir = N | E | S | W deriving (Eq, Show)

if' True  x _ = x
if' False _ y = y

type Tubes = [String]
type Packet = ((Int, Int), Dir, String)

charAtPos :: Tubes -> (Int, Int) -> Char
charAtPos m (r,c) = (m!!r)!!c

appendChar :: Packet -> Char -> Packet
appendChar ((x,y), d, s) c = ((x,y), d, s ++ [c])

updatePos :: Packet -> Packet
updatePos ((x,y), d, s)
  | d == N = ((x-1, y), d, s)
  | d == E = ((x, y+1), d, s)
  | d == S = ((x+1, y), d, s)
  | d == W = ((x, y-1), d, s)

updateDir :: Tubes -> Packet -> Packet
updateDir m ((x,y), d, s)
  | c /= '+'  = ((x,y), d, s)
  | d == N = if' ec ((x,y), E, s) ((x,y), W, s)
  | d == E = if' sc ((x,y), S, s) ((x,y), N, s)
  | d == S = if' wc ((x,y), W, s) ((x,y), E, s)
  | d == W = if' nc ((x,y), N, s) ((x,y), S, s)
  where c = charAtPos m (x,y)
        nc = not $ charAtPos m (x-1,y) `elem` " +"
        ec = not $ charAtPos m (x,y+1) `elem` " +"
        sc = not $ charAtPos m (x+1,y) `elem` " +"
        wc = not $ charAtPos m (x,y-1) `elem` " +"

tick :: Tubes -> Packet -> Packet
tick m ((x,y), d, s)
  | c `elem` ['A'..'Z'] = appendChar p' c
  | otherwise           = p'
  where c = charAtPos m (x,y)
        p' = updateDir m $ updatePos ((x,y), d, s) 

runUpto :: Int -> Tubes -> Packet -> Packet
runUpto n m ((x,y), d, s)
  | length s == n = ((x,y), d, s)
  | otherwise     = runUpto n m $ tick m ((x,y), d, s)

runCount :: Int -> Tubes -> (Packet, Int) -> (Packet, Int)
runCount n m (((x,y), d, s), count)
  | length s == n = (((x,y), d, s), count)
  | otherwise     = runCount n m $ (tick m ((x,y), d, s), count + 1)

main = do
  f <- readFile "input_19.txt"
  let m = lines f

  -- the train's entry point
  let entry = fromJust $ elemIndex '|' (head m)
  let p = ((0, entry), S, "")
  -- the number of character on the map
  let k = length $ filter (\x -> x `elem` ['A'..'Z'])$  concat m 

  putStr "Part 1: "
  let (pos,d,word) = runUpto k m p
  putStrLn word

  putStr "Part 1: "
  let (_, cnt) = runCount k m (p,0)
  putStrLn $ show cnt
