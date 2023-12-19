import Data.Map (Map, findWithDefault, fromList, keys)
import Data.Map qualified as Map

data Direction = L | R deriving (Read)

type Node = (String, Successors)

type Successors = (String, String)

parseDirections :: String -> [Direction]
parseDirections = map (\c -> read [c])

parseNodes :: [String] -> Map String Successors
parseNodes = fromList . map parseNode

parseNode :: String -> Node
parseNode str = (name, (left, right))
  where
    name = take 3 str
    left = take 3 . drop 7 $ str
    right = take 3 . drop 12 $ str

getSuccessor :: Successors -> Direction -> String
getSuccessor (l, _) L = l
getSuccessor (_, r) R = r

step :: Map String Successors -> Direction -> String -> String
step nodes dir name = getSuccessor (nodes Map.! name) dir

countStepsToZZZ :: Map String Successors -> [Direction] -> String -> Int
countStepsToZZZ _ _ "ZZZ" = 0
countStepsToZZZ nodes (d : ds) name = 1 + countStepsToZZZ nodes ds name'
  where
    name' = step nodes d name

part1 :: [String] -> Int
part1 contents = countStepsToZZZ nodes dirs "AAA"
  where
    dirs = cycle $ parseDirections $ head contents
    nodes = parseNodes $ drop 2 contents

countStepsToEndNode :: Map String Successors -> [Direction] -> String -> Integer
countStepsToEndNode nodes (d : ds) name
  | isEndNode name = 0
  | otherwise = 1 + countStepsToEndNode nodes ds name'
  where
    isEndNode = (== 'Z') . last
    name' = step nodes d name

part2 :: [String] -> Integer
part2 contents = foldl1 lcm $ map (countStepsToEndNode nodes dirs) starts
  where
    dirs = cycle $ parseDirections $ head contents
    nodes = parseNodes $ drop 2 contents
    starts = filter isStartNode $ keys nodes
    isStartNode = (== 'A') . last

main = do
  contents <- lines <$> readFile "input.txt"
  print $ part1 contents
  print $ part2 contents
