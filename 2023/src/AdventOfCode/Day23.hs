module AdventOfCode.Day23 (solution) where

import AdventOfCode.Grid (Grid)
import AdventOfCode.Grid qualified as Grid
import AdventOfCode.Position (invert)
import AdventOfCode.Position qualified as Position
import AdventOfCode.Prelude
import Control.Monad.State.Strict (runState)
import Control.Monad.State.Strict qualified as State
import Data.Bits (bit, (.&.), (.|.))
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Data.Vector.Unboxed qualified as Unboxed
import Text.Printf (printf)

type Graph = Vector (Unboxed.Vector (Int, Int, Bool))

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
longestPath graph end = go 0 (BitSet 0) 0
  where
    go :: Int -> BitSet -> Int -> Int
    go !n !seen !i
      | i == end = n
      | i `member` seen = 0
      | otherwise =
          Unboxed.maximum $
            Unboxed.map
              (\(steps, to, _) -> go (n + steps) s to)
              (graph Vector.! i)
      where
        s = insert i seen

-- mapper = if i > 0 then parMap rseq else map

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
                    (\(steps, p, slippery) -> (steps, nodes Map.! p, slippery))
                    edges
          State.put (Map.insert pos (Map.size seen) seen)
          vs <- concat <$> traverse (\(_, p, _) -> go p) edges
          pure (v : vs)

walkToJunction ::
  Grid Unboxed.Vector Char ->
  Position ->
  Direction ->
  Maybe (Int, Position, Bool)
walkToJunction grid position direction =
  let pos = Position.move direction position
   in case Grid.index grid pos of
        Nothing -> Nothing
        Just tile -> guard (tile /= '#') $> go 1 False pos direction tile
  where
    go :: Int -> Bool -> Position -> Direction -> Char -> (Int, Position, Bool)
    go !steps !slippery pos dir tile = case neighbors grid pos dir of
      [(p, d, t)] -> go (steps + 1) (slippery || isslippery tile d) p d t
      _ -> (steps, pos, slippery)

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

isslippery :: Char -> Direction -> Bool
isslippery tile dir = case tile of
  '^' -> dir /= North
  '>' -> dir /= East
  'v' -> dir /= South
  '<' -> dir /= West
  _ -> False

newtype BitSet = BitSet Word
  deriving (Eq)

instance Show BitSet where
  show (BitSet w) = printf "%064b" w

insert :: Int -> BitSet -> BitSet
insert i (BitSet w) = BitSet $ w .|. bit i

member :: Int -> BitSet -> Bool
member i (BitSet w) = w .&. bit i /= 0
