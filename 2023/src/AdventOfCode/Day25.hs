module AdventOfCode.Day25 (solution) where

import AdventOfCode.Parser qualified as Parser
import AdventOfCode.Prelude
import Control.Monad.State.Strict (runState)
import Control.Monad.State.Strict qualified as State
import Data.Char (isAsciiLower)
import Data.HashMap.Lazy qualified as HashMap
import Data.List qualified as List
import Data.Ord (comparing)
import Data.Vector.Storable qualified as Vector
import Numeric.LinearAlgebra (Matrix)
import Numeric.LinearAlgebra qualified as LA

solution :: Solution
solution =
  Solution
    { parser = parseLine `sepEndBy` Parser.endOfLine,
      part1 = solve1,
      part2 = const @Int 0
    }

parseLine :: Parser (ByteString, [ByteString])
parseLine = do
  name <- parseName <* ": "
  connections <- parseName `sepEndBy'` Parser.char ' '
  pure (name, connections)

parseName :: Parser ByteString
parseName = Parser.takeWhile1 isAsciiLower

solve1 :: [(ByteString, [ByteString])] -> Int
solve1 input = size * (n - size)
  where
    adj = adjacencyMatrix input
    n = LA.rows adj
    size = round $ Vector.sum cut
    pairs =
      let m = 12
       in LA.assoc (n, m) 0.0 $ do
            i <- [0 .. m - 1]
            [((0, i), 1.0), ((n - 1 - i, i), -1.0)]
    weights = foldl' (\v a -> v + 0.5 * (a <> v)) pairs (replicate 15 adj)
    cut =
      head . List.maximumBy (comparing length) . List.group . List.sort $
        LA.toColumns (LA.step weights)

adjacencyMatrix :: [(ByteString, [ByteString])] -> Matrix Double
adjacencyMatrix xs = LA.assoc (nodes, nodes) 0.0 edges
  where
    (edges, (_, nodes)) = runState (concat <$> traverse go xs) (HashMap.empty, 0)
    nodeIndex n = do
      (ns, i) <- State.get
      case HashMap.lookup n ns of
        Just j -> pure j
        Nothing -> do
          State.put (HashMap.insert n i ns, i + 1)
          pure i
    go (name, connections) = do
      i <- nodeIndex name
      js <- traverse nodeIndex connections
      pure $ do
        j <- js
        [((i, j), 1.0), ((j, i), 1.0)]
