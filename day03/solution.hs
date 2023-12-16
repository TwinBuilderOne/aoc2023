import Data.Char (isDigit)

type Schematic = [String]

type Coord = (Int, Int)

type Number = (Coord, Int)

type Box = (Coord, Coord)

type Symbol = (Coord, Char)

type Star = Coord

findSymbols :: Schematic -> [Symbol]
findSymbols = go 0 0
  where
    go _ _ [] = []
    go r _ ([] : lists) = go (r + 1) 0 lists
    go r c ((x : xs) : lists)
      | isDigit x || (x == '.') = go r (c + 1) (xs : lists)
      | otherwise = ((r, c), x) : go r (c + 1) (xs : lists)

findNumbers :: Schematic -> [Number]
findNumbers = go 0 0
  where
    go _ _ [] = []
    go r _ ([] : lists) = go (r + 1) 0 lists
    go r c (list@(x : xs) : lists)
      | isDigit x = ((r, c), number) : go r c' (xs' : lists)
      | otherwise = go r (c + 1) (xs : lists)
      where
        number = (read :: String -> Int) . takeWhile isDigit $ list
        offset = length . show $ number
        c' = c + offset
        xs' = drop offset list

box :: Number -> Box
box ((r, c), number) = ((r - 1, c - 1), (r + 1, c + (length . show $ number)))

within :: Box -> Coord -> Bool
within ((r1, c1), (r2, c2)) (r, c) = (r >= r1) && (r <= r2) && (c >= c1) && (c <= c2)

isNextToSymbol :: [Symbol] -> Number -> Bool
isNextToSymbol symbols number = any (within (box number) . fst) symbols

part1 :: [String] -> Int
part1 contents = sum . map snd . filter (isNextToSymbol symbols) $ numbers
  where
    numbers = findNumbers contents
    symbols = findSymbols contents

findStars :: [String] -> [Star]
findStars = map fst . filter isStar . findSymbols
  where
    isStar (_, c) = c == '*'

adjacent :: Star -> Number -> Bool
adjacent star number = within (box number) star

gearRatio :: [Number] -> Star -> Int
gearRatio numbers star = case length adjacentNumbers of
  2 -> product . map snd $ adjacentNumbers
  _ -> 0
  where
    adjacentNumbers = filter (adjacent star) numbers

part2 :: [String] -> Int
part2 contents = sum . map (gearRatio numbers) $ findStars contents
  where
    numbers = findNumbers contents

main = do
  contents <- lines <$> readFile "input.txt"
  print $ part1 contents
  print $ part2 contents
