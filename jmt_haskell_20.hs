import Data.List
import Data.List.Split

type Particle = ([Integer], [Integer], [Integer])

fst' (x, _, _) = x

-- p=<-3811,3483,1459>, v=<162,-112,-26>, a=<9,-12,-8>
parseLine :: String -> Particle
parseLine s = (parseToVector (ss!!0),
               parseToVector (ss!!1),
               parseToVector (ss!!2))
  where ss = splitOn ", " s

-- v=<162,-112,-26>
parseToVector :: String -> [Integer]
parseToVector s = map (read) q :: [Integer]
  where ss = tail . init $ drop 2 s
        q = splitOn "," ss

tick :: Particle -> Particle
tick (pos, vel, acc) = (newPos, newVel, acc)
  where newVel = zipWith (+) vel acc
        newPos = zipWith (+) pos newVel

distFrom0 :: Particle -> Integer
distFrom0 (pos, _, _) = sum $ map (abs) pos

space :: Integer -> [(Particle, Integer)] -> [(Particle, Integer)]
space n xs
  | n == 0    = xs
  | otherwise = space (n-1) xs'
  where xs' = map (\(x, _) -> (tick x, distFrom0 $ tick x)) xs

space2 :: Integer -> [Particle] -> [Particle]
space2 n xs
  | n == 0    = xs
  | otherwise = space2 (n-1) xs'
  where newParts = map (tick) xs
        xs' = filter (\x -> count newParts x < 2) newParts

count :: [Particle] -> Particle -> Integer 
count [] p = 0
count (x:xs) p
  | fst' x == fst' p = 1 + count xs p
  | otherwise        = count xs p

closest :: [(Particle, Integer)] -> (Particle, Integer)
closest [x] = x
closest (x:y:xs)
  | snd x < snd y = closest (x:xs)
  | otherwise     = closest (y:xs)

main = do
  f <- readFile "input_20.txt"
  let t = map (parseLine) $ lines f
  let rr = zip t (map (distFrom0) t)
  let rp = space 400 rr
  let i = elemIndex (closest rp) rp
  putStr "Part 1: "
  putStrLn $ show i
  let j = space2 300 t
  putStr "Part 2: "
  putStrLn . show $ length j

