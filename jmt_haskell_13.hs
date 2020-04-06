import Data.List
import Data.List.Split

type Registry = (Integer, Integer, Integer, Bool)
type FireWall = (Integer, [Registry])

if' True  x _ = x
if' False _ y = y

regId (i,_,_,_) = i
regPos (_,p,_,_) = p
reglen (_,_,l,_) = l

updateReg :: Registry -> Registry
updateReg (i, p, l, dir) = (i, p', l, dir')
  where dir'
          | (p == 0)     = True
          | (p == l - 1) = False
          | otherwise    = dir
        p' = if' dir' (p + 1) (p - 1)

updateFireWall :: FireWall -> FireWall
updateFireWall (p, regs) = (p + 1, map (updateReg) regs)

regAt :: Integer -> [Registry] -> Bool
regAt n regs = (length x == 1)
  where x = filter (\x -> regId x == n) regs

getScannerAt :: Integer -> [Registry] -> Registry
getScannerAt n regs = head $ filter (\x -> (regId x) == n) regs

runDDoS :: FireWall -> Integer
runDDoS (p, regs)
  | p > maxReg + 1 = 0
  | otherwise = score + (runDDoS $ updateFireWall (p, regs))
  where maxReg = maximum $ map regId regs
        score = fireWallScore (p, regs)

fireWallScore :: FireWall -> Integer
fireWallScore (p, regs)
  | nextRegExists = if' (regPos nextReg == 0) (regId nextReg * reglen nextReg) 0
  | otherwise     = 0
  where nextRegExists = regAt (p + 1) regs
        nextReg = getScannerAt (p + 1) regs

removeDelays :: Registry -> [Integer] -> [Integer]
removeDelays (i, _, l, _) xs = [ x | x <- xs, x `mod` p /= i']
  where p = 2*(l-1)
        i' = (-i) `mod` p

parseInput :: String -> Registry
parseInput s = (lhs, 0, rhs, True)
  where s' = splitOn ": " s
        lhs = read (head s') :: Integer
        rhs = read (last s') :: Integer

main = do 
  f <- readFile "input_13.txt"
  let i = map (parseInput) $ lines f

  putStr "Part 1: "
  let q = runDDoS (-1, i)
  putStrLn $ show q
  putStr "Part 2: "
  let s = head $ foldr removeDelays [0,1..] i
  putStrLn $ show s
