import Data.List
import Data.List.Split

import Debug.Trace

type Regs = [(Char, Integer)]
data Command = ADD | JGZ | MUL | MOD | RCV | SND | SET deriving (Eq, Show)
type Dual = (Char, Integer, Bool) -- true = use char
nullDual = ('.', -1, False)
type Instruction = (Command, Dual, Dual)
-- (registries, position, in queue, out queue)
type Machine = (Regs, Int, [Integer], [Integer])

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
  | cmd == "add" = (ADD, d1, d2)
  | cmd == "jgz" = (JGZ, d1, d2)
  | cmd == "mul" = (MUL, d1, d2)
  | cmd == "mod" = (MOD, d1, d2)
  | cmd == "rcv" = (RCV, d1, nullDual)
  | cmd == "snd" = (SND, d1, nullDual)
  | cmd == "set" = (SET, d1, d2)
  | otherwise = error s
  where s' = splitOn " " s
        cmd = s'!!0 -- three char string
        d1 = charToDual (s'!!1)
        d2 = charToDual (s'!!2)
        -- ch = head $ s'!!1 -- single char
        -- t = s'!!2
        -- b = (head t) `elem` ['a'..'z']
        -- v = if' (b) (-1) (read (t) :: Integer)
        -- d = (head t, v , b)

evalInt :: Regs -> Dual -> Integer
evalInt rs (c, v, b)
  | not b     = v
  | otherwise = getRegAt rs c

evalChar :: Dual -> Char
evalChar (c, _, b)
  | b         = c
  | otherwise = error "Should Never Be Hit"

getRegAt :: Regs -> Char -> Integer
getRegAt rs c = snd . head $ dropWhile (\x -> fst x /= c) rs

setRegAt :: Regs -> Char -> Integer -> Regs
setRegAt rs c v = takeWhile (\x -> fst x /= c) rs ++ [(c, v)] ++ tail (dropWhile (\x -> fst x /= c) rs)

unpack :: Regs -> Integer -> Integer
unpack rs v = if' (v < 0) (getRegAt rs (toEnum $ fromIntegral (-1 * v))) (v)

apply :: Regs -> Instruction -> Regs
apply rs (cmd, reg1, dual)
  | cmd == ADD = cmdAdd rs reg v2
  | cmd == MOD = cmdMod rs reg v2
  | cmd == MUL = cmdMul rs reg v2
  | cmd == SET = cmdSet rs reg v2
  where reg = evalChar reg1
        v2 = evalInt rs dual

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
tick (rs, p, inQ, outQ) (cmd, dual1, dual2)
  | cmd == SND = (rs, p + 1, inQ, outQ ++ [v1])
  | cmd == RCV = (cmdSet rs c1 (head inQ), p + 1, tail inQ, outQ)
  | cmd == JGZ = (rs, p', inQ, outQ)
  | otherwise  = (apply rs (cmd, dual1, dual2), p + 1, inQ, outQ)
  where v2 = fromIntegral $ evalInt rs dual2
        v1 = evalInt rs dual1
        c1 = evalChar dual1
        p' = if' (v1 > 0) (p + v2) (p + 1)

run :: Machine -> [Instruction] -> Machine
run (rs, p, inQ, outQ) ins
  | (p - 1) > length ins            = (rs, p, inQ, outQ)
  | cmd == RCV && (length inQ == 0) = (rs, p, inQ, outQ)
  -- | cmd == RCV && (length inQ > 0)  = (rs, p, inQ, outQ)
  | otherwise = run (tick (rs, p, inQ, outQ) (cmd, reg, dual)) ins
  where (cmd, reg, dual) = ins!!p

synch :: [Instruction] -> (Machine, Machine) -> (Machine, Machine)
synch ins ((r1, p1, i1, o1), (r2, p2, i2, o2))
  | length i1 == 0 = ((r1, p1, i1, o1), (r2, p2, i2, o2))
  | otherwise      = synch ins ((r1', p1', o2', []), (r2', p2', i2', o2'))
  where (r1', p1', i1', o1') = run (r1, p1, i1, []) ins
        (r2', p2', i2', o2') = run (r2, p2, o1', []) ins

synch' :: [Instruction] -> (Machine, Machine) -> (Machine, Machine)
synch' ins ((r1, p1, i1, o1), (r2, p2, i2, o2))
  | length i1 == 0 = ((r1, p1, i1, o1), (r2, p2, i2, o2))
  | otherwise      = ((r1', p1', o2', []), (r2', p2', i2', o2'))
  where (r1', p1', i1', o1') = run (r1, p1, i1, []) ins
        (r2', p2', i2', o2') = run (r2, p2, o1', []) ins

getOutputs :: (Machine, Machine) -> ([Integer], [Integer])
getOutputs ((_,_,_,o1), (_,_,_,o2)) = (o1, o2)

recordSends :: (Machine, Machine) -> Int
recordSends (_, (_,_,_,o2)) = length o2

main = do 
  f <- readFile "input_18.txt"
  let ins = map (parseLine) $ lines f

  -- let ins = [(SET, 'a', 1, False), (ADD, 'a', 2, False),
  --            (MUL, 'a', -1, True), (MOD, 'a', 5, False),
  --            (SND, 'a', 0, False), (SET, 'a', 0, False),
  --            (RCV, 'a', 0, False), (JGZ, 'a', -1, False),
  --            (SET, 'a', 1, False), (JGZ, 'a', -2, False)]

  let rs = zip ['a','b','f','i','p'] $ replicate 5 0
  let m = (rs, 0, [],[]) :: Machine

  putStr "Part 1: "
  let (regs, ptr, inQ, outQ) = run m ins
  putStrLn . show . last $ outQ

  putStr "Part 2: "
  let rs1 = cmdSet regs 'p' 0
  let rs2 = cmdSet regs 'p' 1
  -- run once to get a usable state for synch
  let (r1, p1, i1, o1) = run (rs1, 0, [], []) ins
  let (r2, p2, i2, o2) = run (rs2, 0, o1, []) ins
  -- record how many times we've sent a message
  let p0 = length o2
  -- keep repeating synch until there are no inputs
  let k' = drop 1 $ iterate (synch' ins) ((r1, p1, o2, []), (r2, p2, [], []))
  let p = takeWhile(\(_, (_, _, _, o)) -> length o > 0) k'
  -- add these to the first lot of inputs
  putStrLn $ show ((sum $ map (recordSends) p) + p0)
