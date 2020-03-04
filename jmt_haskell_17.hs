import qualified Data.Sequence as Seq
import qualified Data.Foldable as Fold

click :: (Int, Int, Seq.Seq Int) -> (Int, Int, Seq.Seq Int)
click (i, pos, xs) = (i', pos', Seq.insertAt pos' i' xs)
  where i' = i + 1
        pos' = 1 + ((pos + 344) `mod` (length xs))

main = do 
  putStr "Part 1: "
  let t = (0,0, Seq.fromList [0])
  let (_,_,xs) = last . take 2018 $ iterate click t
  let xx = Fold.toList xs
  putStrLn . show . last . take 2 $ dropWhile (\x -> x /= 2017) xx

  putStr "Part 2: "
  let (_,_,ys) = last . take 50000000 $ iterate click t
  let yy = Fold.toList ys
  putStrLn . show . last . take 2 $ dropWhile (\x -> x /= 0) yy
