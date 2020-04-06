import Data.Char

parseInput [] = []
parseInput (s:ss) = [digitToInt s] ++ parseInput ss

if' True  x _ = x
if' False _ y = y

captcha x = captcha' x + (if' (head x == last x)) (head x) 0
  where captcha' [a] = 0
        captcha' (a:b:c) = (if' (a == b) a 0) + captcha' (b:c)

captcha2 x = sum . map (fst) $ filter (\x -> (fst x) == (snd x)) z
  where s = div (length x) 2
        x' = (drop s x) ++ (take s x)
        z = zip x x'

main = do 
  f <- readFile "input_01.txt"
  let t = parseInput . head $ lines f
  putStr "Part 1: "
  putStrLn . show $ captcha t
  putStr "Part 2: "
  putStrLn . show $ captcha2 t
