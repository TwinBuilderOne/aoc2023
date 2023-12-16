import Data.List (intersect)

type Card = ([Int], [Int])

type Owned = [(Int, Int)]

split :: String -> String -> [String]
split _ [] = []
split delims list@(x : xs)
  | isDelim x = split delims xs
  | otherwise = part : split delims rest
  where
    isDelim = (`elem` delims)
    part = takeWhile (not . isDelim) list
    rest = dropWhile isDelim $ dropWhile (not . isDelim) list

parseCard :: String -> Card
parseCard text = (winning, nums)
  where
    parts = split ":|" text
    winsPart = parts !! 1
    numsPart = parts !! 2
    winning = map (read :: String -> Int) . split " " $ winsPart
    nums = map (read :: String -> Int) . split " " $ numsPart

matches :: Card -> Int
matches = length . uncurry intersect

score :: Card -> Int
score card
  | m > 0 = 2 ^ (m - 1)
  | otherwise = 0
  where
    m = matches card

part1 :: [String] -> Int
part1 = sum . map (score . parseCard)

iterator :: Owned -> Owned
iterator ((matches, owned) : rest) = received ++ same
  where
    received = map (\(m, o) -> (m, o + owned)) (take matches rest)
    same = drop matches rest

part2 :: [String] -> Int
part2 contents = sum . map (snd . head) $ takeWhile (/= []) $ iterate iterator initial
  where
    initial = map ((,1) . matches) cards
    cards = map parseCard contents

main = do
  contents <- lines <$> readFile "input.txt"
  print $ part1 contents
  print $ part2 contents
