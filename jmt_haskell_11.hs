import Data.List
import Data.List.Split
import Data.Map (fromListWith, toList)

data SIXTH = ID | U1 | U2 | U3 | U4 | U5 deriving (Ord, Show, Eq)

toSixth :: String -> SIXTH
toSixth s
  | s == "n"  = ID
  | s == "nw" = U1
  | s == "sw" = U2
  | s == "s"  = U3
  | s == "se" = U4
  | s == "ne" = U5

parseInput s = map (toSixth) $ splitOn "," s

frequency :: (Ord a) => [a] -> [(a, Int)]
frequency xs = toList (fromListWith (+) [(x, 1) | x <- xs])

getCount :: [(SIXTH, Int)] -> SIXTH -> Int
getCount xs s
  | length t > 0 = snd $ head t
  | otherwise    = 0
  where t = filter(\x -> fst x == s) xs

updateCounts :: Int -> [SIXTH] -> [(SIXTH, Int)] -> [(SIXTH, Int)]
updateCounts n ss xs = xs'
  where xs' = map (fn) xs
        fn (sixth, count)
          | sixth `elem` ss = (sixth, count + n)
          | otherwise       = (sixth, count)

reduce1 :: [SIXTH] -> [(SIXTH, Int)] -> [(SIXTH, Int)]
reduce1 ss xs = updateCounts (-1 * rmv) ss xs
  where rmv = minimum $ map (getCount xs) ss

reduce2 :: ((SIXTH, SIXTH), SIXTH) -> [(SIXTH, Int)] -> [(SIXTH, Int)]
reduce2 ((a,b),c) xs = xs''
  where shift = min (getCount xs a) (getCount xs b)
        xs' = updateCounts (-1 * shift) [a,b] xs
        xs'' = updateCounts (shift) [c] xs'

sixthReduce :: [(SIXTH, Int)] -> [(SIXTH, Int)]
sixthReduce xs = xs''
  where xs' = foldr (reduce1) xs [[ID, U2, U4], [U1, U3, U5],
                                  [U2, U5], [U1, U4], [ID, U3]]
        xs'' = foldr (reduce2) xs' [((U5, U1), ID), ((ID, U2), U1),
                                    ((U1, U3), U2), ((U2, U4), U3),
                                    ((U3, U5), U4), ((U4, ID), U5)]

getLength :: [SIXTH] -> Int
getLength xs = sum $ map snd h
  where h = sixthReduce $ frequency xs

main = do 
  f <- readFile "jmt_input_11.txt"
  let i = parseInput . head $ lines f
  putStr "Part 1: "
  putStrLn . show $ getLength i

  let i' = inits i
  let c' = map (getLength) i'
  putStr "Part 2: "
  putStrLn . show $ maximum c'
