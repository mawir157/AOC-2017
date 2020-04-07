import qualified Data.Sequence as Seq
import qualified Data.Foldable as Fold

if' True  x _ = x
if' False _ y = y

click :: (Int, Int, Seq.Seq Int) -> (Int, Int, Seq.Seq Int)
click (i, pos, xs) = (i', pos', Seq.insertAt pos' i' xs)
  where i' = i + 1
        pos' = 1 + ((pos + 328) `mod` (length xs))

-- i = 0
-- for t in xrange(1,50000000+1):
--   i = (i+step)%t + 1
--   if i==1:
--     val_after_0 = t
-- print val_after_0

click' :: (Int, Int, Int) -> (Int, Int, Int)
click' (!i, !t, !val) = (i', (t+1), val')
  where !i' = ((i + 328) `mod` t) + 1
        !val' = if' (i' == 1) t val

rep :: Int -> (Int, Int, Int) -> (Int, Int, Int)
rep 0 p = p
rep !n !p = rep (n-1) (click' p)

-- repNew :: Int -> Int -> Int -> Int
-- repNew 50000001 _ val = val
-- repNew n ptr val =
--     let !newPtr = ((ptr + 328) `mod` n) + 1
--         !newval = if' (newPtr == 1) n val
--     in repNew (n + 1) newPtr newval

main = do 
  putStr "Part 1: "
  let t = (0,0, Seq.fromList [0])
  let (_,_,xs) = iterate click t !! 2018
  let xx = Fold.toList xs
  putStrLn . show . last . take 2 $ dropWhile (\x -> x /= 2017) xx

  putStr "Part 2: "
  let s = 50000000
  let k = rep s (0, 1, 0)
  putStrLn $ show k
  -- let j = repNew 1 0 0
  -- putStrLn $ show j
