type Race = (Integer, Integer)

split :: [Char] -> String -> [String]
split _ [] = []
split delims list@(x : xs)
  | isDelim x = split delims xs
  | otherwise = part : split delims rest
  where
    isDelim = (`elem` delims)
    part = takeWhile (not . isDelim) list
    rest = dropWhile isDelim $ dropWhile (not . isDelim) list

parseRaces :: [String] -> [Race]
parseRaces contents = zip times distances
  where
    times = parseTimesDistances $ head contents
    distances = parseTimesDistances $ last contents

parseTimesDistances :: String -> [Integer]
parseTimesDistances = map read . tail . split " "

-- t(T - t) > D
-- -t^2 + Tt - D = 0
-- a = -1, b = T, c = -D
solveRace :: Race -> Integer
solveRace race = maxEnd - minEnd + 1
  where
    t = fromIntegral $ fst race
    d = fromIntegral $ snd race
    minFrac = (-t + sqrt (t * t - 4 * d)) / (-2)
    maxFrac = (-t - sqrt (t * t - 4 * d)) / (-2)
    minEnd = ceiling minFrac
    maxEnd = floor maxFrac

part1 :: [String] -> Integer
part1 = product . map solveRace . parseRaces

parseRaces2 :: [String] -> Race
parseRaces2 contents = (toNum $ head contents, toNum $ last contents)
  where
    toNum = read . filter (/= ' ') . last . split ":"

part2 :: [String] -> Integer
part2 = solveRace . parseRaces2

main = do
  contents <- lines <$> readFile "input.txt"
  print $ part1 contents
  print $ part2 contents
