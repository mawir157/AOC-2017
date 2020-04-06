import Data.List.Split

data Move = Spin | Exch | Part deriving (Eq, Show)
type Command = (Move, (Int, Int))

parseCommand :: String -> Command
parseCommand (s:ss)
  | s == 's' = (Spin, (read ss :: Int, 0))
  | s == 'x' = (Exch, (read (k!!0) :: Int, read (k!!1) :: Int))
  | s == 'p' = (Part, (fromEnum (head $ head k), fromEnum (head $ last k)))
  where k = splitOn "/" ss

parseInput :: String -> [Command]
parseInput s = map (parseCommand) (splitOn "," s)

swapElementsAt :: Int -> Int -> [Char] -> [Char]
swapElementsAt i j xs = left ++ [elemHi] ++ middle ++ [elemLo] ++ right
  where elemLo = xs !! (min i j)
        elemHi = xs !! (max i j)
        left = take (min i j) xs
        middle = take ((max i j) - (min i j) - 1) (drop ((min i j) + 1) xs)
        right = drop ((max i j) + 1) xs

swapLetters :: Int -> Int -> [Char] -> [Char]
swapLetters x y ss = map (replace (toEnum x) (toEnum y)) ss
  where replace x' y' c
          | c == x'   = y'
          | c == y'   = x'
          | otherwise = c

applyCommand :: [Char] -> Command -> [Char]
applyCommand ss (m, (x,y))
  | m == Spin = drop n ss ++ take n ss
  | m == Exch = swapElementsAt x y ss
  | m == Part = swapLetters x y ss
  where n = length ss - x

getOrd :: [Char] -> [Char] -> [Command] -> Int
getOrd match prev dance 
  | match == next = 1
  | otherwise     = 1 + getOrd match next dance
  where next = foldl applyCommand prev dance

repList :: Int -> [a] -> [a]
repList n xs = take (n * length xs) $ cycle xs

main = do 
  f <- readFile "input_16.txt"
  let i = parseInput f
  let t = ['a','b'..'p']

  putStr "Part 1: "
  putStrLn . show $ foldl applyCommand t i

  putStr "Part 2: "
  -- We break the dance into two COMMUTATIVE parts - PERMUATION + RENAMING 
  -- The permutation part has order
  let ord_p = getOrd t t $ filter (\(x,_) -> x == Spin || x == Exch) i
  -- The relabelling part has order
  let ord_r = getOrd t t $ filter (\(x,_) -> x == Part) i
  -- The whole dance has order
  let ord = lcm ord_p ord_r

  -- Rather than do 10^9 repetitions we only have to do
  let reps = 1000000000 `mod` ord
  putStrLn . show $ foldl applyCommand t (repList reps i)
