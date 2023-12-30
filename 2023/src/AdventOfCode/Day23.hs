module AdventOfCode.Day23 (solution) where

import AdventOfCode.Grid (Grid)
import AdventOfCode.Grid qualified as Grid
import AdventOfCode.Position (invert)
import AdventOfCode.Position qualified as Position
import AdventOfCode.Prelude
import Control.Exception (assert)
import Control.Parallel.Strategies (parMap, rseq)
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Data.List qualified as List
import Data.Ord (comparing)
import Data.Vector.Unboxed (Vector)

type Graph = HashMap Position [Edge]

type Node = Position

data Edge = Edge {steps :: !Int, to :: !Node, slippy :: !Bool}
  deriving (Eq, Show)

solution :: Solution
solution =
  Solution
    { parser = Grid.parse,
      solver = solve1 &&& solve2
    }

solve1 :: Grid Vector Char -> Int
solve1 grid =
  longestPath start end $
    HashMap.map (filter (not . slippy)) graph
  where
    graph = buildGraph start grid
    start = fromMaybe (error "no start") $ Grid.findPosition (== '.') grid
    end = List.maximumBy (comparing row) $ HashMap.keys graph

solve2 :: Grid Vector Char -> Int
solve2 grid = longestPath start end graph
  where
    graph = buildGraph start grid
    start = fromMaybe (error "no start") $ Grid.findPosition (== '.') grid
    end = List.maximumBy (comparing row) $ HashMap.keys graph

buildGraph :: Position -> Grid Vector Char -> Graph
buildGraph start grid = go HashMap.empty start
  where
    go graph pos
      | pos `HashMap.member` graph = graph
      | otherwise =
          let edges = findEdges pos
           in foldl' go (HashMap.insert pos edges graph) (map to edges)
    findEdges :: Position -> [Edge]
    findEdges pos =
      assert (Grid.unsafeIndex grid pos == '.') $ do
        d <- [North, East, South, West]
        let p = Position.move d pos
        case Grid.index grid p of
          Nothing -> []
          Just t
            | t == '#' -> []
            | otherwise -> pure $ walkEdge 1 False p d t
    walkEdge :: Int -> Bool -> Position -> Direction -> Char -> Edge
    walkEdge n slippy pos dir tile = case nexts of
      [(p, d, t)] -> walkEdge (n + 1) (slippy || isSlippy tile d) p d t
      _ -> Edge {steps = n, slippy, to = pos}
      where
        nexts = do
          d <- List.delete (invert dir) [North, East, South, West]
          let p = Position.move d pos
          case Grid.index grid p of
            Nothing -> []
            Just t
              | t == '#' -> []
              | otherwise -> pure (p, d, t)

longestPath :: Position -> Position -> Graph -> Int
longestPath start end graph = go 12 0 HashSet.empty start
  where
    go :: Int -> Int -> HashSet Position -> Position -> Int
    go !i !n seen pos
      | pos == end = n
      | pos `HashSet.member` seen = 0
      | otherwise = case HashMap.lookup pos graph of
          Nothing -> 0
          Just nexts ->
            maximum $
              mapper (\Edge {steps, to} -> go (i - 1) (n + steps) set to) nexts
      where
        set = HashSet.insert pos seen
        mapper = if i > 0 then parMap rseq else map

isSlippy :: Char -> Direction -> Bool
isSlippy tile dir = case tile of
  '^' -> dir /= North
  '>' -> dir /= East
  'v' -> dir /= South
  '<' -> dir /= West
  _ -> False
