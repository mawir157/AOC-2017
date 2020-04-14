import Data.List
import Data.List.Split

type Regs = [(Char, Integer)]
data Command = ADD | JGZ | JNZ | MUL | MOD |
               RCV | SND | SET | SUB deriving (Eq, Show)
type Instruction = (Command, Char, Integer, Char)
type Machine = (Regs, Integer, Int) -- (registries, last frequency, position)

if' True  x _ = x
if' False _ y = y

parseLine :: String -> Instruction
parseLine s
  | cmd == "add" = (ADD, ch, v, p)
  | cmd == "jgz" = (JGZ, ch, v, p)
  | cmd == "jnz" = (JNZ, ch, v, p)--
  | cmd == "mul" = (MUL, ch, v, p)
  | cmd == "mod" = (MOD, ch, v, p)
  | cmd == "rcv" = (RCV, ch, -1, '*')
  | cmd == "snd" = (SND, ch, -1, '*')
  | cmd == "set" = (SET, ch, v, p)
  | cmd == "sub" = (SUB, ch, v, p)--
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
  | cmd == SUB = cmdSub rs reg v'
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

cmdSub :: Regs -> Char -> Integer -> Regs
cmdSub rs c v = setRegAt rs c v'
  where v' = (getRegAt rs c) - v

tick :: Machine -> Instruction -> Machine
tick (rs, freq, p) (cmd, ch, v, vp)
  | cmd == SND = (rs, getRegAt rs ch, p + 1)
  | cmd == RCV = (rs, freq, p + 1)
  | cmd == JGZ = (rs, freq, g')
  | cmd == JNZ = (rs, freq, n')
  | otherwise  = (apply rs (cmd, ch, v, vp), freq, p + 1)
  where v' = fromIntegral $ if' (vp == '*') v (getRegAt rs vp)
        g' = if' ((getRegAt rs ch) > 0) (p + v') (p + 1)
        n' = if' ((getRegAt rs ch) /= 0) (p + v') (p + 1)

run :: Machine -> [Instruction] -> Integer
run (rs, freq, p) ins
  | (p - 1) > length ins = freq
  | cmd == RCV           = freq
  | otherwise            = run (tick (rs, freq, p) (cmd, ch, v, vp)) ins
  where (cmd, ch, v, vp) = head (drop p ins) 

main = do 
  f <- readFile "input_23.txt"
  let ins = map (parseLine) $ lines f

  putStrLn $ show ins

  let rs = zip ['a'..'h'] $ replicate 8 0
  let m = (rs, 0, 0)

  let n = run m ins
  putStrLn $ show n
