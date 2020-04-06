import Data.List
import Data.List.Split

data OP = INC | DEC deriving (Eq, Show)
data COMP = NEQ | EQU | LEQ | LTH | GEQ | GTH deriving (Eq, Show)

type Instruction = (String, (OP, Integer), String, (COMP, Integer))
type Computer = [(String, Integer)]

if' True  x _ = x
if' False _ y = y

toOp s
  | s == "dec" = DEC
  | s == "inc" = INC

toComp s
  | s == "!=" = NEQ
  | s == "==" = EQU
  | s == "<=" = LEQ
  | s == "<"  = LTH
  | s == ">=" = GEQ
  | s == ">"  = GTH

parseInput :: String -> Instruction
parseInput s = (s1, (op, n1), s2, (co, n2))
  where ss = (splitOn " " s)
        s1 = head ss
        op = toOp $ ss!!1
        n1 = read (ss!!2) :: Integer
        s2 = ss!!4
        co = toComp $ ss!!5
        n2 = read (ss!!6) :: Integer

extendMemory :: String -> Computer -> Computer
extendMemory s c
  | s `elem` map (fst) c = c -- if we've arleady seen s, do nothing
  | otherwise            = c ++ [(s, 0)] --otherwise add a new registry set to 0

getRegistry :: Computer -> String -> Integer
getRegistry x s = snd . head $ filter (\x -> fst x == s) x

updateRegistry :: Computer -> String -> Integer -> Computer
updateRegistry x s n = x' ++ [(s, n)]
  where x' = filter (\x -> fst x /= s) x

applyInstr :: Computer -> Instruction -> Computer
applyInstr x (s1, lhs, s2, rhs) = updateRegistry x' s1 newVal
  where x' = extendMemory s1 $ extendMemory s2 x
        reg1 = getRegistry x' s1
        reg2 = getRegistry x' s2
        newVal = if' (checkComp reg2 rhs) (inc reg1 lhs) reg1

checkComp :: Integer -> (COMP, Integer) -> Bool
checkComp x (comp, y)
  | comp == NEQ = (x /= y) 
  | comp == EQU = (x == y) 
  | comp == LEQ = (x <= y) 
  | comp == LTH = (x <  y) 
  | comp == GEQ = (x >= y) 
  | comp == GTH = (x >  y)

inc :: Integer -> (OP, Integer) -> Integer
inc x (op, y)
  | op == INC = x + y
  | op == DEC = x - y

main = do 
  f <- readFile "input_08.txt"
  let t = map (parseInput) $ lines f
  putStr "Part 1: "
  let r = foldl (applyInstr) [] t
  putStrLn . show . maximum $ map (snd) r

  putStr "Part 2: "
  let s = scanl (applyInstr) [] t
  let s' = drop 1 $ map (map snd) s 
  putStrLn . show . maximum $ map (maximum) s'
