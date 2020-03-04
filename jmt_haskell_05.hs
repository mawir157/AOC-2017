import qualified Data.Vector as Vec

type Prg = (Int, Vec.Vector Int)

if' True  x _ = x
if' False _ y = y

parseInput x = map (read) x :: [Int]

tick (n,v)
  | n + 1 > length v = (n,v)
  | otherwise        = (n + p, v Vec.// [(n, p + 1)])
  where p = v Vec.! n

tick' (n,v)
  | n + 1 > length v = (n,v)
  | otherwise        = (n + p, v Vec.// [(n, p')])
  where p = v Vec.! n
        p' = if' (p >= 3) (p-1) (p+1)

run (c, p)
  | p == p'   = (c, p)
  | otherwise = run (c+1, p')
  where p' = tick p

run' (c, p)
  | p == p'   = (c, p)
  | otherwise = run' (c+1, p')
  where p' = tick' p

main = do 
  f <- readFile "jmt_input_05.txt"
  let t = Vec.fromList . parseInput $ lines f
  putStr "Part 1: "
  let p = (0, t)
  let s = run (0, p)
  putStrLn . show $ fst s

  putStr "Part 2: "
  let r = run' (0, p)
  putStrLn . show $ fst r
