module AdventOfCode.Day23 (solution) where

import AdventOfCode.Grid (Grid)
import AdventOfCode.Grid qualified as Grid
import AdventOfCode.Position (invert)
import AdventOfCode.Position qualified as Position
import AdventOfCode.Prelude
import Control.Exception (assert)
import Control.Monad.State.Strict (runState)
import Control.Monad.State.Strict qualified as State
import Control.Parallel.Strategies (parMap, rseq)
import Data.Bits (setBit, testBit)
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Vector (Vector, (!))
import Data.Vector qualified as Vector
import Data.Vector.Unboxed qualified as Unboxed
import Data.Word (Word64)
import Text.Printf (printf)

type Steps = Int

type Node = Int

type Graph = Vector (Unboxed.Vector (Node, Steps, Bool))

solution :: Solution
solution =
  Solution
    { parser = Grid.parse,
      solver = solve
    }

solve :: Grid Unboxed.Vector Char -> (Int, Int)
solve grid =
  ( longestPath (Vector.map (Unboxed.filter (not . slippery)) graph) goal,
    longestPath graph goal
  )
  where
    (graph, goal) = buildGraph start grid
    start = fromMaybe (error "no start") $ Grid.findPosition (== '.') grid
    slippery (_, _, b) = b

longestPath :: Graph -> Int -> Int
longestPath graph end =
  assert (Vector.length graph < 64) $ go 8 0 (BitSet 0) 0
  where
    go :: Int -> Int -> BitSet -> Int -> Int
    go !j !n !seen !i
      | i == end = n
      | j > 0 =
          foldl' max 0 . parMap rseq f . Unboxed.toList $
            Unboxed.filter p (graph ! i)
      | otherwise =
          Unboxed.foldl' max 0 . Unboxed.map f $
            Unboxed.filter p (graph ! i)
      where
        f (node, steps, _) = go (j - 1) (n + steps) (insert i seen) node
        p (node, _, _) = not (member node seen)

buildGraph :: Position -> Grid Unboxed.Vector Char -> (Graph, Int)
buildGraph start grid = (Vector.fromList graph, snd $ Map.findMax nodes)
  where
    (graph, nodes) = runState (go start) Map.empty
    go pos = do
      seen <- State.get
      if pos `Map.member` seen
        then pure []
        else do
          let edges =
                mapMaybe
                  (walkToJunction grid pos)
                  [North, East, South, West]
              v =
                Unboxed.fromList $
                  map
                    (\(p, steps, slippery) -> (nodes Map.! p, steps, slippery))
                    edges
          State.put (Map.insert pos (Map.size seen) seen)
          vs <- concat <$> traverse (\(p, _, _) -> go p) edges
          pure (v : vs)

walkToJunction ::
  Grid Unboxed.Vector Char ->
  Position ->
  Direction ->
  Maybe (Position, Steps, Bool)
walkToJunction grid position direction =
  let pos = Position.move direction position
   in case Grid.index grid pos of
        Nothing -> Nothing
        Just tile -> guard (tile /= '#') $> go 1 False pos direction tile
  where
    go !steps !slippery pos dir tile = case neighbors grid pos dir of
      [(p, d, t)] -> go (steps + 1) (slippery || isSlippery tile d) p d t
      _ -> (pos, steps, slippery)

neighbors ::
  Grid Unboxed.Vector Char ->
  Position ->
  Direction ->
  [(Position, Direction, Char)]
neighbors grid pos dir = do
  d <- List.delete (invert dir) [North, East, South, West]
  let p = Position.move d pos
  case Grid.index grid p of
    Nothing -> []
    Just t -> [(p, d, t) | t /= '#']

isSlippery :: Char -> Direction -> Bool
isSlippery tile dir = case tile of
  '^' -> dir /= North
  '>' -> dir /= East
  'v' -> dir /= South
  '<' -> dir /= West
  _ -> False

newtype BitSet = BitSet Word64
  deriving (Eq)

instance Show BitSet where
  show (BitSet w) = printf "%064b" w

insert :: Int -> BitSet -> BitSet
insert i (BitSet w) = BitSet $ setBit w i

member :: Int -> BitSet -> Bool
member i (BitSet w) = testBit w i
