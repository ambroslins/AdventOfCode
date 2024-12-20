module AdventOfCode.Day20 (solution) where

import AdventOfCode.Grid (Grid)
import AdventOfCode.Grid qualified as Grid
import AdventOfCode.Position qualified as Pos
import AdventOfCode.Prelude
import Control.Monad (forM_, when)
import Control.Monad.ST.Strict (runST)
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as MVU
import Debug.Trace (traceShow)
import Deque.Strict qualified as Deque
import GHC.IsList (fromList)

solution :: Solution
solution =
  Solution
    { parser = Grid.parse,
      solver = solve
    }

bfs :: Grid VU.Vector Bool -> Position -> Grid VU.Vector Int
bfs track start = Grid.create $ do
  times <- Grid.newMutable @VU.Vector nrows ncols (-1 :: Int)

  let race queue = case Deque.uncons queue of
        Nothing -> pure ()
        Just ((t, pos), q) -> do
          t' <- Grid.read times pos
          if t' >= 0
            then race q
            else do
              Grid.write times pos t
              let nexts =
                    [ (t + 1, p)
                    | dir <- [North, East, South, West],
                      let p = Pos.move dir pos,
                      Grid.unsafeIndex track p
                    ]
              race (q <> fromList nexts)
  race $ fromList [(0, start)]
  pure times
  where
    (nrows, ncols) = Grid.size track

solve :: Grid VU.Vector Char -> (Int, Int)
solve grid = (cheat 2, cheat 20)
  where
    (nrows, ncols) = Grid.size grid
    start = fromJust $ Grid.findPosition (== 'S') grid
    end = fromJust $ Grid.findPosition (== 'E') grid
    track = Grid.map (/= '#') grid
    timesStart = bfs track start
    timesEnd = bfs track end
    timeNoCheat = Grid.unsafeIndex timesStart end
    cheat n = runST $ do
      saves <- MVU.replicate timeNoCheat (0 :: Int)
      forM_ [1 .. nrows - 2] $ \row -> forM_ [1 .. ncols - 2] $ \col -> do
        let p1 = Position {row, col}
            t1start = Grid.unsafeIndex timesStart p1
        when (t1start >= 0) $
          forM_ [-n .. n] $ \dr ->
            forM_ [-n + abs dr .. n - abs dr] $ \dc ->
              let p2 = Position {row = row + dr, col = col + dc}
               in case Grid.index timesEnd p2 of
                    Nothing -> pure ()
                    Just t2end -> do
                      let save1 = timeNoCheat - t1start - t2end - abs dr - abs dc
                      when (Grid.inside p2 track && t2end >= 0 && save1 > 0) $
                        MVU.modify saves (+ 1) save1

      MVU.foldl' (+) 0 $ MVU.drop 100 saves
