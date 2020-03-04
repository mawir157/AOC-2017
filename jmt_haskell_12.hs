import Data.List
import Data.List.Split

type Network = [Integer]
type Map = (Integer, Network)
type Mapping = (Network,[Map])

parseInput :: String -> Map
parseInput s = (lhs, rhs)
  where s' = splitOn " <-> " s
        lhs = read (head s') :: Integer
        rhs = map (read) (splitOn ", " $ last s') :: [Integer]

addToNetwork :: Mapping -> Mapping
addToNetwork (ss, ms) = (ss', ms')
  where toAdd = concat . map snd $ filter (\x -> fst x `elem` ss) ms
        ss' = nub (ss ++ toAdd)
        ms' = filter (\x -> not (fst x `elem` ss)) ms

buildNetwork :: Mapping -> Mapping
buildNetwork m
  | m == m'   = m
  | otherwise = buildNetwork m'
  where m' = addToNetwork m

buildAllNetworks :: Mapping -> [Network]
buildAllNetworks (ss, ms)
  | length ms > 0 = [ss'] ++ buildAllNetworks ([fst . head $ ms'], ms')
  | otherwise     = []
  where (ss', ms') = buildNetwork (ss, ms)

main = do 
  f <- readFile "jmt_input_12.txt"
  let i = map (parseInput) $ lines f
  putStr "Part 1: "
  let (s', i') = buildNetwork ([0], i)
  putStrLn . show . length $ s'
  putStr "Part 2: "
  let t = buildAllNetworks ([0], i)
  putStrLn . show $ length t
