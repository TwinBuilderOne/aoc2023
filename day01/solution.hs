import Data.Char (digitToInt, isDigit)
import Data.Text (Text, pack, replace, unpack)

digits :: String -> [Int]
digits = map digitToInt . filter isDigit

combineDigits :: String -> Int
combineDigits line = head (digits line) * 10 + last (digits line)

part1 :: [String] -> Int
part1 = sum . map combineDigits

numbers :: [(Int, String)]
numbers =
  [ (1, "one"),
    (2, "two"),
    (3, "three"),
    (4, "four"),
    (5, "five"),
    (6, "six"),
    (7, "seven"),
    (8, "eight"),
    (9, "nine")
  ]

replacements :: [(Text, Text)]
replacements = map fn numbers
  where
    fn (x, str) = (pack str, pack $ str ++ show x ++ str)

extend :: String -> String
extend str = unpack $ foldl (flip (uncurry replace)) (pack str) replacements

part2 :: [String] -> Int
part2 = part1 . map extend

main = do
  contents <- lines <$> readFile "input.txt"
  print $ part1 contents
  print $ part2 contents
