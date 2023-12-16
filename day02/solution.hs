{-# LANGUAGE OverloadedStrings #-}

import Data.Text (Text, pack, splitOn, toTitle, unpack)

data Color = Red | Green | Blue deriving (Read, Show)

parseLine :: Text -> (Int, [(Int, Color)])
parseLine line = (parseId firstPart, parsePulls secondPart)
  where
    parts = splitOn ": " line
    firstPart = head parts
    secondPart = last parts

parseId :: Text -> Int
parseId = read . unpack . last . splitOn " "

parsePulls :: Text -> [(Int, Color)]
parsePulls text = map parsePull $ concatMap (splitOn ", ") (splitOn "; " text)

parsePull :: Text -> (Int, Color)
parsePull pull = (read num, read color)
  where
    parts = splitOn " " pull
    num = unpack $ head parts
    color = unpack . toTitle $ last parts

part1 :: [Text] -> Int
part1 = sum . map fst . filter (all possible . snd) . map parseLine

possible :: (Int, Color) -> Bool
possible (x, Red) = x <= 12
possible (x, Green) = x <= 13
possible (x, Blue) = x <= 14

part2 :: [Text] -> Int
part2 = sum . map (power . snd . parseLine)

power :: [(Int, Color)] -> Int
power = prod . foldl powerAcc (0, 0, 0)

powerAcc :: (Int, Int, Int) -> (Int, Color) -> (Int, Int, Int)
powerAcc xs pull = edit xs (snd pull) (max (fst pull) (get xs (snd pull)))

get :: (Int, Int, Int) -> Color -> Int
get (x, _, _) Red = x
get (_, y, _) Green = y
get (_, _, z) Blue = z

edit :: (Int, Int, Int) -> Color -> Int -> (Int, Int, Int)
edit (_, y, z) Red val = (val, y, z)
edit (x, _, z) Green val = (x, val, z)
edit (x, y, _) Blue val = (x, y, val)

prod :: (Int, Int, Int) -> Int
prod (x, y, z) = x * y * z

main = do
  contents <- map pack . lines <$> readFile "input.txt"
  print $ part1 contents
  print $ part2 contents
