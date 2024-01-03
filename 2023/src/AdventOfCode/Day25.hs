module AdventOfCode.Day25 (solution) where

import AdventOfCode.Graph (Edge, Graph, Node)
import AdventOfCode.Graph qualified as Graph
import AdventOfCode.Parser qualified as Parser
import AdventOfCode.Prelude
import AdventOfCode.Search (bfsOnInt)
import Control.Monad ((>=>))
import Data.ByteString.Char8 qualified as BS
import Data.Char (isAsciiLower, ord)
import Data.IntMap qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.List qualified as List
import Data.Vector.Unboxed qualified as Vector

solution :: Solution
solution =
  Solution
    { parser = parseLine `sepEndBy'` Parser.endOfLine,
      solver = solve
    }

parseLine :: Parser (ByteString, [ByteString])
parseLine = do
  name <- parseName <* ": "
  connections <- parseName `sepEndBy'` Parser.char ' '
  pure (name, connections)

parseName :: Parser ByteString
parseName = Parser.takeWhile1 isAsciiLower

solve :: [(ByteString, [ByteString])] -> (Int, Int)
solve input =
  ( maybe (error "no cut") (\m -> m * (n - m)) part1,
    0
  )
  where
    graph = buildGraph input
    n = Vector.length (Graph.nodes graph)
    part1 = findFirst (uncurry $ minimumCut graph) $ do
      i : is <- List.tails [0 .. n - 1]
      guard $ Vector.length (Graph.neighbours graph i) > 3
      j <- is
      pure (i, j)

minimumCut :: Graph -> Node -> Node -> Maybe Int
minimumCut graph start end =
  case (cut >=> cut >=> cut) IntSet.empty of
    Left _ -> error "no path after one or two cuts"
    Right used -> case cut used of
      Left n -> Just n
      Right _ -> Nothing
  where
    cut !used =
      foldl' (flip IntSet.insert) used
        <$> shortestPath used graph start end

shortestPath :: IntSet -> Graph -> Node -> Node -> Either Int [Edge]
shortestPath used graph start end =
  sizeOrPath 0 $ bfsOnInt fst (uncurry next) [(start, [])]
  where
    next !node !path =
      mapMaybe (useEdge path) . Vector.toList $
        Graph.neighbours graph node
    useEdge !path (!edge, !node)
      | edge `IntSet.member` used = Nothing
      | otherwise = Just (node, edge : path)
    sizeOrPath !n = \case
      [] -> Left n
      ((node, path) : rest)
        | node == end -> Right path
        | otherwise -> sizeOrPath (n + 1) rest

buildGraph :: [(ByteString, [ByteString])] -> Graph
buildGraph =
  Graph.fromIntMap . IntMap.fromListWith (<>) . concatMap f
  where
    f (name, connections) =
      let h = hash name
          hs = map hash connections
       in (h, hs) : map (,[h]) hs
    hash = BS.foldl' (\h c -> h * 26 + (ord c - ord 'a')) 0
