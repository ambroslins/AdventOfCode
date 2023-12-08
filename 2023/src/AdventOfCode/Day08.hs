module AdventOfCode.Day08 (solution) where

import AdventOfCode.Parser qualified as Parser
import AdventOfCode.Prelude
import Data.ByteString.Char8 qualified as BS
import Data.Char (isAsciiUpper)
import Data.HashMap.Strict ((!))
import Data.HashMap.Strict qualified as HashMap
import Data.List (findIndex, scanl')

type Steps = [Node -> Node]

type Label = ByteString

data Node = Node
  { label :: !ByteString,
    left :: Node,
    right :: Node
  }

instance Show Node where
  show Node {label} = BS.unpack label

solution :: Solution
solution =
  Solution
    { parser = do
        steps <- parseSteps <* Parser.endOfLine <* Parser.endOfLine
        nodes <- parseNode `sepEndBy` Parser.endOfLine
        pure (steps, makeNodeMap nodes),
      part1 = uncurry solve1,
      part2 = count (BS.isSuffixOf "A") . HashMap.keys . snd
    }

parseSteps :: Parser Steps
parseSteps = Parser.takeWhile1 isAsciiUpper <&> BS.foldr step []
  where
    step 'L' steps = left : steps
    step 'R' steps = right : steps
    step c _ = error $ "parseSteps: invalid step " <> show c

parseNode :: Parser (Label, (Label, Label))
parseNode = do
  name <- Parser.take 3
  Parser.symbol " = ("
  l <- Parser.take 3
  Parser.symbol ","
  r <- Parser.take 3
  Parser.symbol ")"
  pure (name, (l, r))

makeNodeMap :: [(Label, (Label, Label))] -> HashMap Label Node
makeNodeMap nodes = nodeMap
  where
    nodeMap = HashMap.fromList $ map makeNode nodes
    makeNode (label, (l, r)) =
      ( label,
        Node {label, left = nodeMap ! l, right = nodeMap ! r}
      )

walkNetwork :: Node -> Steps -> [Node]
walkNetwork = scanl' (flip ($))

countStepsTo :: Node -> Steps -> Label -> Int
countStepsTo node steps target =
  fromMaybe
    (error $ "countStepsTo: target " <> show target <> " not found")
    $ findIndex isTarget
    $ walkNetwork node (cycle steps)
  where
    isTarget Node {label} = label == target

solve1 :: Steps -> HashMap Label Node -> Int
solve1 steps nodeMap = countStepsTo (nodeMap ! "AAA") steps "ZZZ"
