import Text.Regex
import Text.Regex.Posix

regexRemove :: String -> String -> String
regexRemove s k = subRegex (mkRegex k) s ""

hordeTheTrash :: String -> [String]
hordeTheTrash s = getAllTextMatches $ s =~ "<[^>]*>" :: [String]

santize :: String -> String
santize s = foldl regexRemove s ["!.", "<[^>]*>", ","]

score :: (Integer, String, Integer) -> (Integer, String, Integer)
score (i, [], p) = (i, [], p)
score (i, (s:ss), p)
  | s == '{'  = score (i + 1, ss, p)
  | s == '}'  = score (i - 1, ss, p + i)
  | otherwise = score (i, ss, p)

main = do 
  f <- readFile "input_09.txt"
  let i = head $ lines f
  let (_,_,s) = score (0, santize i, 0)
  putStr "Part 1: "
  putStrLn $ show s
  putStr "Part 2: "
  let i' = hordeTheTrash $ regexRemove i "!."
  putStrLn . show . sum $ map (\x -> length x - 2) i'
