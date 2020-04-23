import qualified Data.Sequence as Seq

data Direction = L | R | N deriving (Eq, Show)
data Label = HALT | X | A | B | C | D | E | F deriving (Eq, Show)

--(tape, head, current Label)
type TuringState = ([Char], Int, Label)

--(cur, scan, print, move, next)
type Instruction  = (Label, Char, Char, Direction, Label)
type TuringMachine = [Instruction]

atHead :: TuringState -> Char
atHead (tape, hd, _) = tapeRead hd tape

getCur :: TuringState -> Label
getCur (_, _, c) = c

findInstruction :: TuringState -> TuringMachine -> Instruction
findInstruction ts xs = head xs'
  where scanned = atHead ts
        cur = getCur ts
        xs' = dropWhile(\(a,b,_,_,_) -> (a /= cur) || (b /= scanned)) xs

---------------------------- Code for updating tape ----------------------------
tapeRead :: Int -> [Char] -> Char
tapeRead n xs
  | (n >= length xs) || (n < 0) = error ("read past the end of the tape: " ++ (show n))
  | otherwise     = head $ drop n xs

tapeWrite :: Int -> Char -> [Char] -> [Char]
tapeWrite n x xs
  | n >= length xs || (n < 0) = error ("write past the end of the tape: " ++ (show n))
  | otherwise      = (take n xs) ++ [x] ++ drop (n + 1) xs

updateTape :: Int -> Instruction -> [Char] -> [Char]
updateTape hd (_, _, p, _, _) tape
  | p == 'N'  = tape                  -- do nothing
  | p == 'E'  = tapeWrite hd '_' tape -- erase character at head
  | otherwise = tapeWrite hd p tape   -- print p at at head
----------------------------- Code for moving head -----------------------------
moveHead :: Int -> Instruction -> Int
moveHead hd (_, _, _, m, _)
  | m == R    = hd + 1
  | m == L    = hd - 1
  | otherwise = hd
--------------------------------------------------------------------------------
getNext :: Instruction -> Label
getNext (_, _, _, _, n) = n
--------------------------------------------------------------------------------
run :: TuringState -> TuringMachine -> TuringState
run ts tm
  | getCur ts == HALT = ts -- hit the halting condition
  | otherwise         = run (apply ts ins) tm
  where ins = findInstruction ts tm

runFor :: Integer -> TuringState -> TuringMachine -> TuringState
runFor n ts tm
  | n <= 0            = ts
  | getCur ts == HALT = ts -- hit the halting condition
  | otherwise         = runFor (n-1) (apply ts ins) tm
  where ins = findInstruction ts tm

apply :: TuringState -> Instruction -> TuringState
apply (tape, hd, cur) ins
  | cur == HALT = error "Halted!"
  | otherwise  = (tape', hd', cur')
  where tape' = updateTape hd ins tape
        hd' = moveHead hd ins
        cur' = getNext ins

-- this has been inputed by hand :(
bb5 = [(A,'0','1',R,B),
       (A,'1','0',L,D),
       (B,'0','1',R,C),
       (B,'1','0',R,F),
       (C,'0','1',L,C),
       (C,'1','1',L,A),
       (D,'0','0',L,E),
       (D,'1','1',R,A),
       (E,'0','1',L,A),
       (E,'1','0',R,B),
       (F,'0','0',R,C),
       (F,'1','0',R,E)]

main = do
  let p = 10000
  let ts = ((replicate (p) '0'), 25, A)

  let (ts',_,_) = runFor 12302209 ts bb5
  putStrLn $ show ts'
  putStrLn . show . length $ filter (=='1') ts'