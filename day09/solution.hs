split :: [Char] -> String -> [String]
split _ [] = []
split delims list@(x : xs)
  | isDelim x = split delims xs
  | otherwise = part : split delims rest
 where
  isDelim = (`elem` delims)
  part = takeWhile (not . isDelim) list
  rest = dropWhile isDelim $ dropWhile (not . isDelim) list

parseLine :: String -> [Integer]
parseLine = map read . split " "

solveLine :: [Integer] -> Integer
solveLine [] = 0
solveLine xs = last xs + solveLine (zipWith (-) (tail xs) xs)

part1 :: [String] -> Integer
part1 = sum . map (solveLine . parseLine)

part2 :: [String] -> Integer
part2 = sum . map (solveLine . reverse . parseLine)

main = do
  contents <- lines <$> readFile "input.txt"
  print $ part1 contents
  print $ part2 contents
