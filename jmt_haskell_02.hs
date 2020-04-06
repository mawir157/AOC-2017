import Data.List
import Data.List.Split

parseInput [] = []
parseInput (x:xs) = [reverse $ sort n] ++ parseInput xs
  where n = map (read) (splitOn "\t" x) :: [Integer]

checkSum [] = 0
checkSum (x:xs) = absDiff x + checkSum xs
  where absDiff x = (maximum x) - (minimum x)

checkSum2 x = sum . map (\t -> fst t `div` snd t) $ getDivs x
  where getDivs [] = []
        getDivs (x:xs) = [head . filter (\t -> (fst t `mod` snd t) == 0) $ getPairs x] ++ getDivs xs

getPairs [x] = []
getPairs (x:xs) = map (\t -> (x, t)) xs ++ getPairs xs

main = do 
  f <- readFile "input_02.txt"
  let t = parseInput $ lines f
  putStr "Part 1: "
  putStrLn . show $ checkSum t
  putStr "Part 2: "
  putStrLn . show $ checkSum2 t