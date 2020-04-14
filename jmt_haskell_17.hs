import qualified Data.Sequence as Seq
import qualified Data.Foldable as Fold

if' True  x _ = x
if' False _ y = y

click :: (Int, Int, Seq.Seq Int) -> (Int, Int, Seq.Seq Int)
click (i, pos, xs) = (i', pos', Seq.insertAt pos' i' xs)
  where i' = i + 1
        pos' = 1 + ((pos + 328) `mod` (length xs))

rep :: Int -> (Int, Int, Int) -> (Int, Int, Int)
rep 0 p = p
rep n (i, t, val) = rep (n-1) (i', (t+1), val')
  where i' = ((i + 328) `mod` t) + 1
        !val' = if' (i' == 1) t val

main = do 
  putStr "Part 1: "
  let t = (0,0, Seq.fromList [0])
  let (_,_,xs) = iterate click t !! 2018
  let xx = Fold.toList xs
  putStrLn . show . last . take 2 $ dropWhile (\x -> x /= 2017) xx

  putStr "Part 2: "
  let s = 50000000
  let (_,_,k) = rep s (0, 1, 0)
  putStrLn $ show k
