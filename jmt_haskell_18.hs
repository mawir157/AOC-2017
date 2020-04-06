import Data.List
import Data.List.Split

type Regs = [(Char, Integer)]
data Command = ADD | JGZ | MUL | MOD | RCV | SND | SET deriving (Eq, Show)
type Instruction = (Command, Char, Integer, Char)
type Machine = (Regs, Integer, Int) -- (registries, last frequency, position)

if' True  x _ = x
if' False _ y = y

parseLine :: String -> Instruction
parseLine s
  | cmd == "add" = (ADD, ch, v, p)
  | cmd == "jgz" = (JGZ, ch, v, p)
  | cmd == "mul" = (MUL, ch, v, p)
  | cmd == "mod" = (MOD, ch, v, p)
  | cmd == "rcv" = (RCV, ch, -1, '*')
  | cmd == "snd" = (SND, ch, -1, '*')
  | cmd == "set" = (SET, ch, v, p)
  | otherwise = error s
  where s' = splitOn " " s
        cmd = s'!!0 -- three char string
        ch = head $ s'!!1 -- single char
        t = s'!!2
        b = (head t) `elem` ['a'..'z']
        v = if' (b) (-1) (read (t) :: Integer)
        p = if' (b) (head t) '*'

getRegAt :: Regs -> Char -> Integer
getRegAt rs c = snd . head $ dropWhile (\x -> fst x /= c) rs

setRegAt :: Regs -> Char -> Integer -> Regs
setRegAt rs c v = takeWhile (\x -> fst x /= c) rs ++ [(c, v)] ++ tail (dropWhile (\x -> fst x /= c) rs)

unpack :: Regs -> Integer -> Integer
unpack rs v = if' (v < 0) (getRegAt rs (toEnum $ fromIntegral (-1 * v))) (v)

apply :: Regs -> Instruction -> Regs
apply rs (cmd, reg, v, vp)
  | cmd == ADD = cmdAdd rs reg v'
  | cmd == MOD = cmdMod rs reg v'
  | cmd == MUL = cmdMul rs reg v'
  | cmd == SET = cmdSet rs reg v'
  where v' = if' (vp == '*') v (getRegAt rs vp)

cmdAdd :: Regs -> Char -> Integer -> Regs
cmdAdd rs c v = setRegAt rs c v'
  where v' = (getRegAt rs c) + v

cmdMod :: Regs -> Char -> Integer -> Regs
cmdMod rs c v = setRegAt rs c v'
  where v' = (getRegAt rs c) `mod` v

cmdMul :: Regs -> Char -> Integer -> Regs
cmdMul rs c v = setRegAt rs c v'
  where v' = (getRegAt rs c) * v

cmdSet :: Regs -> Char -> Integer -> Regs
cmdSet rs c v = setRegAt rs c v

tick :: Machine -> Instruction -> Machine
tick (rs, freq, p) (cmd, ch, v, vp)
  | cmd == SND = (rs, getRegAt rs ch, p + 1)
  | cmd == RCV = (rs, freq, p + 1)
  | cmd == JGZ = (rs, freq, p')
  | otherwise  = (apply rs (cmd, ch, v, vp), freq, p + 1)
  where v' = fromIntegral $ if' (vp == '*') v (getRegAt rs vp)
        p' = if' ((getRegAt rs ch) > 0) (p + v') (p + 1)

run :: Machine -> [Instruction] -> Integer
run (rs, freq, p) ins
  | (p - 1) > length ins = freq
  | cmd == RCV     = freq
  | otherwise      = run (tick (rs, freq, p) (cmd, ch, v, vp)) ins
  where (cmd, ch, v, vp) = head (drop p ins) 

main = do 
  f <- readFile "input_18.txt"
  let ins = map (parseLine) $ lines f

  -- let ins = [(SET, 'a', 1, '*'),  (ADD, 'a', 2, '*'), (MUL, 'a', -1, 'a'),
  --            (MOD, 'a', 5, '*'),
  --            (SND, 'a', 0, '*'),  (SET, 'a', 0, '*'), (RCV, 'a', 0, '*'),
  --            (JGZ, 'a', -1, '*'), (SET, 'a', 1, '*'), (JGZ, 'a', -2, '*')]

  let rs = zip ['a','b','f','i','p'] $ replicate 5 0
  let m = (rs, 0, 0)

  --putStrLn $ show m
  --putStrLn $ show ins

  let n = run m ins
  putStrLn $ show n

  --putStrLn $ show ins

-- set a 1
-- add a 2
-- mul a a
-- mod a 5
-- snd a
-- set a 0
-- rcv a
-- jgz a -1
-- set a 1
-- jgz a -2

--     The first four instructions set a to 1, add 2 to it, square it, and then set it to itself modulo 5, resulting in a value of 4.
--     Then, a sound with frequency 4 (the value of a) is played.
--     After that, a is set to 0, causing the subsequent rcv and jgz instructions to both be skipped (rcv because a is 0, and jgz because a is not greater than 0).
--     Finally, a is set to 1, causing the next jgz instruction to activate, jumping back two instructions to another jump, which jumps again to the rcv, which ultimately triggers the recover operation.
