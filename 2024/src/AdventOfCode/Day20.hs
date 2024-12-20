module AdventOfCode.Day20 (solution) where

import AdventOfCode.Grid (Grid)
import AdventOfCode.Grid qualified as Grid
import AdventOfCode.Position qualified as Pos
import AdventOfCode.Prelude
import Control.Monad (when)
import Control.Monad.ST.Strict (runST)
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as MVU
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
solve grid = (cheat 2, cheat 20)
  where
    (!nrows, !ncols) = Grid.size grid
    !start = fromJust $ Grid.findPosition (== 'S') grid
    !end = fromJust $ Grid.findPosition (== 'E') grid
    !track = Grid.map (/= '#') grid
    !timesStart = bfs track start end
    !timesEnd = bfs track end start
    !timeNoCheat = Grid.unsafeIndex timesStart end
    cheat n = runST $ do
      saves <- MVU.replicate timeNoCheat (0 :: Int)
      for 1 (nrows - 2) $ \row -> for 1 (ncols - 2) $ \col -> do
        let !pStart = Position {row, col}
            !tStart = Grid.unsafeIndex timesStart pStart
        when (tStart >= 0) $
          for (max (-n) (1 - row)) (min n (nrows - row - 2)) $ \dr ->
            for (max (abs dr - n) (1 - col)) (min (n - abs dr) (ncols - col - 2)) $ \dc ->
              let !pEnd = Position {row = row + dr, col = col + dc}
                  !tEnd = Grid.unsafeIndex timesEnd pEnd
                  save = timeNoCheat - tStart - tEnd - abs dr - abs dc
               in when (tEnd >= 0 && save > 0) $ MVU.modify saves (+ 1) save

      MVU.foldl' (+) 0 $ MVU.drop 100 saves
