module AdventOfCode.Day20 (solution) where

import AdventOfCode.Grid (Grid)
import AdventOfCode.Grid qualified as Grid
import AdventOfCode.Position qualified as Pos
import AdventOfCode.Prelude
import Control.Parallel.Strategies (parMap, rseq)
import Data.Monoid (Sum (..))
import Data.Vector.Unboxed qualified as VU
import Deque.Strict qualified as Deque
import GHC.IsList (fromList)

solution :: Solution
solution =
  Solution
    { parser = Grid.parse,
      solver = solve
    }

bfs :: Grid VU.Vector Bool -> Position -> Position -> Grid VU.Vector Int
bfs track start end = Grid.create $ do
  times <- Grid.newMutable @VU.Vector nrows ncols (-1 :: Int)

  let race queue = case Deque.uncons queue of
        Nothing -> pure ()
        Just (Pair t pos, q)
          | pos == end -> Grid.write times pos t
          | otherwise -> do
              t' <- Grid.read times pos
              if t' >= 0
                then race q
                else do
                  Grid.write times pos t
                  let nexts =
                        [ Pair (t + 1) p
                        | dir <- [North, East, South, West],
                          let p = Pos.move dir pos,
                          Grid.unsafeIndex track p
                        ]
                  race (q <> fromList nexts)
  race $ fromList [Pair 0 start]
  pure times
  where
    (nrows, ncols) = Grid.size track

solve :: Grid VU.Vector Char -> (Int, Int)
solve grid = (part1, part2)
  where
    Pair (Sum part1) (Sum part2) =
      foldMap' id $
        parMap rseq (\p -> Pair (cheat 2 p) (cheat 20 p)) $
          VU.toList $
            Grid.findPositions (>= 0) timesStart
    (!nrows, !ncols) = Grid.size grid
    !start = fromJust $ Grid.findPosition (== 'S') grid
    !end = fromJust $ Grid.findPosition (== 'E') grid
    !track = Grid.map (/= '#') grid
    !timesStart = bfs track start end
    !timesEnd = bfs track end start
    !timeNoCheat = Grid.unsafeIndex timesStart end
    cheat n p1 =
      let t1 = Grid.unsafeIndex timesStart p1
       in foldRange (max (-n) (1 - row p1)) (min n (nrows - row p1 - 2)) 0 $ \acc1 dr ->
            foldRange
              (max (abs dr - n) (1 - col p1))
              (min (n - abs dr) (ncols - col p1 - 2))
              acc1
              $ \acc dc ->
                let !p2 = Position {row = row p1 + dr, col = col p1 + dc}
                    !t2 = Grid.unsafeIndex timesEnd p2
                    save = timeNoCheat - t1 - t2 - abs dr - abs dc
                 in if (t2 >= 0 && save >= 100) then acc + 1 else acc

foldRange :: Int -> Int -> a -> (a -> Int -> a) -> a
foldRange start stop x f = go start x
  where
    go !i !acc
      | i > stop = acc
      | otherwise = go (i + 1) (f acc i)
