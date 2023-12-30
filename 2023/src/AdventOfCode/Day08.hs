module AdventOfCode.Day08 (solution) where

import AdventOfCode.Parser qualified as Parser
import AdventOfCode.Prelude
import Control.Parallel.Strategies (parMap, rseq)
import Data.ByteString.Char8 qualified as BS
import Data.Char (isAsciiUpper)
import Data.HashMap.Strict ((!))
import Data.HashMap.Strict qualified as HashMap

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
      solver = uncurry solve1 &&& uncurry solve2
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

countStepsTo :: (Label -> Bool) -> Node -> Steps -> Int
countStepsTo p node steps = go 0 node steps
  where
    go !i n = \case
      [] -> go i n steps
      (x : xs)
        | p (label n) -> i
        | otherwise -> go (i + 1) (x n) xs

solve1 :: Steps -> HashMap Label Node -> Int
solve1 steps nodeMap =
  countStepsTo (== "ZZZ") (nodeMap ! "AAA") steps

solve2 :: Steps -> HashMap Label Node -> Int
solve2 steps nodeMap = foldl' lcm 1 counts
  where
    starts = filter (BS.isSuffixOf "A" . label) $ HashMap.elems nodeMap
    counts = parMap rseq (\n -> countStepsTo (BS.isSuffixOf "Z") n steps) starts
