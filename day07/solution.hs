import Data.Char (isDigit)
import Data.List (sort, sortBy)
import Data.Map (Map, assocs, delete, findWithDefault, fromListWith)
import Data.Ord (Down (..), comparing)

data Hand = Hand HandType [Card] deriving (Eq, Ord)

data HandType
  = HighCard
  | Pair
  | TwoPair
  | Three
  | FullHouse
  | Four
  | Five
  deriving (Eq, Ord)

data Card
  = Joker
  | Num Int
  | Ten
  | Jack
  | Queen
  | King
  | Ace
  deriving (Eq, Ord)

split :: [Char] -> String -> [String]
split _ [] = []
split delims list@(x : xs)
  | isDelim x = split delims xs
  | otherwise = part : split delims rest
  where
    isDelim = (`elem` delims)
    part = takeWhile (not . isDelim) list
    rest = dropWhile isDelim $ dropWhile (not . isDelim) list

parseLine :: String -> (Hand, Int)
parseLine str = (parseHand handPart, read bidPart)
  where
    parts = split " " str
    handPart = head parts
    bidPart = last parts

parseHand :: String -> Hand
parseHand hand = Hand (getType cards) cards
  where
    cards = parseCards hand

parseCards :: String -> [Card]
parseCards = map toCard

toCard :: Char -> Card
toCard 'T' = Ten
toCard 'J' = Jack
toCard 'Q' = Queen
toCard 'K' = King
toCard 'A' = Ace
toCard x
  | isDigit x = Num (read [x])
  | otherwise = error "Invalid card"

getType :: [Card] -> HandType
getType cards = typeMatcher counts
  where
    countMap = fromListWith (+) $ map (,1) cards
    counts = sortBy (comparing Data.Ord.Down) $ map snd $ assocs countMap

typeMatcher :: [Int] -> HandType
typeMatcher (5 : _) = Five
typeMatcher (4 : _) = Four
typeMatcher (3 : 2 : _) = FullHouse
typeMatcher (3 : _) = Three
typeMatcher (2 : 2 : _) = TwoPair
typeMatcher (2 : _) = Pair
typeMatcher _ = HighCard

part1 :: [String] -> Int
part1 = sum . zipWith (*) [1 ..] . map snd . sort . map parseLine

parseLine2 :: String -> (Hand, Int)
parseLine2 str = (parseHand2 handPart, read bidPart)
  where
    parts = split " " str
    handPart = head parts
    bidPart = last parts

parseHand2 :: String -> Hand
parseHand2 hand = Hand (getType2 cards) cards
  where
    cards = parseCards2 hand

parseCards2 :: String -> [Card]
parseCards2 = map toCard2

toCard2 :: Char -> Card
toCard2 'J' = Joker
toCard2 x = toCard x

getType2 :: [Card] -> HandType
getType2 [Joker, Joker, Joker, Joker, Joker] = Five
getType2 cards = typeMatcher alteredCounts
  where
    countMap = fromListWith (+) $ map (,1) cards
    countMapNoJokers = delete Joker countMap
    counts = sortBy (comparing Data.Ord.Down) $ map snd $ assocs countMapNoJokers
    jokersCount = findWithDefault 0 Joker countMap
    alteredCounts = (head counts + jokersCount) : tail counts

part2 :: [String] -> Int
part2 = sum . zipWith (*) [1 ..] . map snd . sort . map parseLine2

main = do
  contents <- lines <$> readFile "input.txt"
  print $ part1 contents
  print $ part2 contents
