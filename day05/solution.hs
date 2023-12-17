type Map = [Rule]

type Rule = (Integer, Integer, Integer)

split :: (Eq a) => [a] -> [a] -> [[a]]
split _ [] = []
split delims list@(x : xs)
  | isDelim x = split delims xs
  | otherwise = part : split delims rest
  where
    isDelim = (`elem` delims)
    part = takeWhile (not . isDelim) list
    rest = dropWhile isDelim $ dropWhile (not . isDelim) list

at :: Int -> [a] -> a
at idx xs = xs !! idx

parseSeeds :: [String] -> [Integer]
parseSeeds = map read . split " " . at 1 . split ":" . head

parseMaps :: [String] -> [Map]
parseMaps = map parseMap . split [""] . drop 2
  where
    parseMap = map parseRule . drop 1
    parseRule = makeRule . map read . split " "
    makeRule xs = (head xs, xs !! 1, last xs)

transform :: Map -> Integer -> Integer
transform [] x = x
transform ((dst, src, len) : rs) x
  | (x >= src) && (x < src + len) = x + dst - src
  | otherwise = transform rs x

part1 :: [String] -> Integer
part1 contents = minimum $ foldl applyMap seeds maps
  where
    seeds = parseSeeds contents
    maps = parseMaps contents
    applyMap xs m = map (transform m) xs

type Interval = (Integer, Integer)

type Rule2 = (Interval, Interval)

type Map2 = [Rule2]

inside :: Interval -> Interval -> Bool
(a, b) `inside` (c, d) = (a >= c) && (b <= d)

snips :: Interval -> Interval -> Bool
(a, b) `snips` (c, d) = (a < c) && (b >= c)

makeIntervals :: [Integer] -> [Interval]
makeIntervals [] = []
makeIntervals (x : y : xs) = (x, x + y - 1) : makeIntervals xs

parseSeeds2 :: [String] -> [Interval]
parseSeeds2 = makeIntervals . map read . split " " . at 1 . split ":" . head

parseMaps2 :: [String] -> [Map2]
parseMaps2 = map parseMap2 . split [""] . drop 2
  where
    parseMap2 = map parseRule2 . drop 1
    parseRule2 = makeRule2 . map read . split " "
    makeRule2 xs = ((xs !! 1, xs !! 1 + last xs - 1), (head xs, head xs + last xs - 1))

apply :: Rule2 -> Interval -> Interval
apply ((src, _), (dst, _)) (a, b) = (a + dst - src, b + dst - src)

transform2 :: Map2 -> Interval -> [Interval]
transform2 m (a, b)
  | enclose /= [] = [apply (head enclose) (a, b)]
  | left /= [] = transform2 m (a, leftSnip) ++ transform2 m (leftSnip + 1, b)
  | right /= [] = transform2 m (a, rightSnip - 1) ++ transform2 m (rightSnip, b)
  | otherwise = [(a, b)]
  where
    enclose = filter (((a, b) `inside`) . fst) m
    left = filter ((`snips` (a, b)) . fst) m
    right = filter (((a, b) `snips`) . fst) m
    leftSnip = snd . fst . head $ left
    rightSnip = fst . fst . head $ right

part2 :: [String] -> Integer
part2 contents = minimum . map fst $ foldl applyMap2 seeds maps
  where
    seeds = parseSeeds2 contents
    maps = parseMaps2 contents
    applyMap2 xs m = concatMap (transform2 m) xs

main :: IO ()
main = do
  contents <- lines <$> readFile "input.txt"
  print $ part1 contents
  print $ part2 contents
