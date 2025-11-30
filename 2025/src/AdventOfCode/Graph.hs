module AdventOfCode.Graph
  ( Graph,
    Node,
    Edge,
    nodes,
    edges,
    empty,
    fromIntMap,
    neighbours,
  )
where

import Control.Monad.State.Strict (evalState, state)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.List qualified as List
import Data.Maybe (fromMaybe)
import Data.Vector.Unboxed (Vector, (!), (!?))
import Data.Vector.Unboxed qualified as Vector

type Node = Int

type Edge = Int

data Graph = Graph
  { -- | A list of edge indices.
    nodes :: !(Vector Edge),
    -- | A list of node indices.
    edges :: !(Vector Node)
  }
  deriving (Eq, Show)

neighbours :: Graph -> Node -> Vector (Edge, Node)
neighbours Graph {nodes, edges} node =
  Vector.zip es (Vector.backpermute edges es)
  where
    start = nodes ! node
    end = fromMaybe (Vector.length edges) $ nodes !? (node + 1)
    es = Vector.iterateN (end - start) (+ 1) start

empty :: Graph
empty = Graph {nodes = Vector.empty, edges = Vector.empty}

fromIntMap :: IntMap [Int] -> Graph
fromIntMap adj
  | IntMap.null adj = empty
  | otherwise =
      Graph
        { nodes = Vector.fromListN (IntMap.size adj) ns,
          edges =
            Vector.fromListN size $
              concatMap (map (indices IntMap.!)) $
                IntMap.elems adj
        }
  where
    (ns, size) =
      unsnoc $ List.scanl' (+) 0 $ map length $ IntMap.elems adj
    indices = evalState (traverse (const inc) adj) 0
    inc = state $ \i -> (i, i + 1)

unsnoc :: [a] -> ([a], a)
unsnoc = \case
  [] -> error "unsnoc: empty list"
  [x] -> ([], x)
  x : xs -> let ~(ys, y) = unsnoc xs in (x : ys, y)
