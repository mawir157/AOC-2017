import Data.List
import Data.List.Split

parseInput [] = []
parseInput (x:xs) = [(splitOn " " x)] ++ parseInput xs

anyReps x = (length x) == (length $ nub x)

anyAnagrams x = (length x) == (length $ nub x')
  where x' = map (sort) x

main = do 
  f <- readFile "input_04.txt"
  let t = parseInput $ lines f
  putStr "Part 1: "
  putStrLn . show . length . filter (\x -> anyReps x) $ t
  putStr "Part 2: "
  putStrLn . show . length . filter (\x -> anyAnagrams x) $ t