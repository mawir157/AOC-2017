import Data.List
import Data.List.Split

import Debug.Trace

type Regs = [(Char, Integer)]
data Command = JNZ | MUL | SET | SUB deriving (Eq, Show)
type Dual = (Char, Integer, Bool) -- true = use char
nullDual = ('.', -1, False)
type Instruction = (Command, Dual, Dual)
-- (registries, position, in queue, out queue)
type Machine = (Regs, Int)

if' True  x _ = x
if' False _ y = y

charToDual :: String -> Dual
charToDual s
  | b         = (c, -1, b)
  | otherwise = ('*', v, b)
  where c = head s
        b = c `elem` ['a'..'z']
        v = if' (b) (-1) (read (s) :: Integer)

parseLine :: String -> Instruction
parseLine s
  | cmd == "jnz" = (JNZ, d1, d2)
  | cmd == "mul" = (MUL, d1, d2)
  | cmd == "set" = (SET, d1, d2)
  | cmd == "sub" = (SUB, d1, d2)
  where s' = splitOn " " s
        cmd = s'!!0 -- three char string
        d1 = charToDual (s'!!1)
        d2 = charToDual (s'!!2)

evalInt :: Regs -> Dual -> Integer
evalInt rs (c, v, b)
  | not b     = v
  | otherwise = getRegAt rs c

evalChar :: Dual -> Char
evalChar (c, _, b) = c

getRegAt :: Regs -> Char -> Integer
getRegAt rs c = snd . head $ dropWhile (\x -> fst x /= c) rs

setRegAt :: Regs -> Char -> Integer -> Regs
setRegAt rs c v = takeWhile (\x -> fst x /= c) rs ++ [(c, v)] ++ tail (dropWhile (\x -> fst x /= c) rs)

unpack :: Regs -> Integer -> Integer
unpack rs v = if' (v < 0) (getRegAt rs (toEnum $ fromIntegral (-1 * v))) (v)

apply :: Regs -> Instruction -> Regs
apply rs (cmd, reg1, dual)
  | cmd == MUL = cmdMul rs reg v2
  | cmd == SET = cmdSet rs reg v2
  | cmd == SUB = cmdSub rs reg v2
  where reg = evalChar reg1
        v2 = evalInt rs dual

cmdMul :: Regs -> Char -> Integer -> Regs
cmdMul rs c v = setRegAt rs c v'
  where v' = (getRegAt rs c) * v

cmdSet :: Regs -> Char -> Integer -> Regs
cmdSet rs c v = setRegAt rs c v

cmdSub :: Regs -> Char -> Integer -> Regs
cmdSub rs c v = setRegAt rs c v'
  where v' = (getRegAt rs c) - v

tick :: Machine -> Instruction -> Machine
tick (rs, p) (cmd, dual1, dual2)
  | cmd == JNZ = (rs, np')
  | otherwise  = (apply rs (cmd, dual1, dual2), p + 1)
  where v2 = fromIntegral $ evalInt rs dual2
        v1 = evalInt rs dual1
        np' = if' (v1 /= 0) (p + v2) (p + 1)

run :: Machine -> [Instruction] -> Machine
run (rs, p) ins
  | p >= length ins = (rs, p)
  | otherwise       = run m' ins
  where m' = tick (rs, p) (cmd, reg, dual)
        (cmd, reg, dual) = ins!!p

runCount :: Command -> (Machine, Integer) -> [Instruction] -> (Machine, Integer)
runCount c ((rs, p), n) ins
  | p >= length ins = ((rs, p), n)
  | otherwise       = runCount c (m', n') ins
  where m' = tick (rs, p) (cmd, reg, dual)
        (cmd, reg, dual) = ins!!p
        n' = if' (cmd == c) (n+1) n

isPrime :: Integer -> Bool
isPrime n = not $ or res
  where factors = takeWhile (\x -> x*x <= n ) [2..]
        res = map (\x -> n `mod` x == 0) factors

main = do 
  f <- readFile "input_23.txt"
  let ins = map (parseLine) $ lines f

  let rs = zip "abcdefgh" $ replicate 8 0
  let m = (rs, 0) :: Machine

  let m' = runCount MUL (m,0) ins

  putStr "Part 1: "
  putStrLn . show $ snd m'

  putStr "Part 2: "
  -- the machine counts the number of non-primes in the arithmetic
  -- progression 106500 + 17*n upto 123500
  -- for a different input file have to change magic numbers
  let rng = [106500, 106517..123500]
  let p = filter (not . isPrime) rng
  putStrLn . show $ length p
