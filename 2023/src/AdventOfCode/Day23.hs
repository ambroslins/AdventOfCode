module AdventOfCode.Day23 (solution) where

import AdventOfCode.Grid (Grid)
import AdventOfCode.Grid qualified as Grid
import AdventOfCode.Position (invert)
import AdventOfCode.Position qualified as Position
import AdventOfCode.Prelude
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.List qualified as List
import Data.Map.Strict ((!))
import Data.Map.Strict qualified as Map
import Data.Vector.Unboxed (Vector)

data Node = Node {nodeId :: !Int, edges :: [Edge Node]}

data Edge a = Edge {steps :: !Int, to :: !a, slippy :: !Bool}
  deriving (Eq, Show, Functor)

solution :: Solution
solution =
  Solution
    { parser = Grid.parse,
      solver = solve
    }

solve :: Grid Vector Char -> (Int, Int)
solve grid =
  ( longestPath False (graph ! start) end,
    longestPath True (graph ! start) end
  )
  where
    graph = buildGraph start grid
    start = fromMaybe (error "no start") $ Grid.findPosition (== '.') grid
    end = nodeId $ snd $ Map.findMax graph

longestPath :: Bool -> Node -> Int -> Int
longestPath slopes start end = go 0 IntSet.empty start
  where
    filt = if slopes then id else filter (not . slippy)
    go :: Int -> IntSet -> Node -> Int
    go n seen Node {nodeId, edges}
      | nodeId == end = n
      | nodeId `IntSet.member` seen = 0
      | otherwise =
          maximum $
            map
              (\Edge {steps, to} -> go (n + steps) seen' to)
              (filt edges)
      where
        seen' = IntSet.insert nodeId seen

-- mapper = if i > 0 then parMap rseq else map

buildGraph :: Position -> Grid Vector Char -> Map Position Node
buildGraph start grid = nodes
  where
    nodes = go Map.empty start
    go ns pos
      | pos `Map.member` ns = ns
      | otherwise = foldl' go (Map.insert pos node ns) (map to edges)
      where
        edges = mapMaybe (walkToJunction grid pos) [North, East, South, West]
        node =
          Node
            { nodeId = Map.size ns,
              edges = map (fmap (nodes !)) edges
            }

walkToJunction ::
  Grid Vector Char ->
  Position ->
  Direction ->
  Maybe (Edge Position)
walkToJunction grid position direction =
  let pos = Position.move direction position
   in case Grid.index grid pos of
        Nothing -> Nothing
        Just tile -> guard (tile /= '#') $> go 1 False pos direction tile
  where
    go :: Int -> Bool -> Position -> Direction -> Char -> Edge Position
    go !steps !slippy pos dir tile = case neighbors grid pos dir of
      [(p, d, t)] -> go (steps + 1) (slippy || isSlippy tile d) p d t
      _ -> Edge {steps, to = pos, slippy}

neighbors :: Grid Vector Char -> Position -> Direction -> [(Position, Direction, Char)]
neighbors grid pos dir = do
  d <- List.delete (invert dir) [North, East, South, West]
  let p = Position.move d pos
  case Grid.index grid p of
    Nothing -> []
    Just t -> [(p, d, t) | t /= '#']

isSlippy :: Char -> Direction -> Bool
isSlippy tile dir = case tile of
  '^' -> dir /= North
  '>' -> dir /= East
  'v' -> dir /= South
  '<' -> dir /= West
  _ -> False
