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

racePath :: Grid VU.Vector Bool -> Position -> Grid VU.Vector Int
racePath track start = Grid.create $ do
  let (nrows, ncols) = Grid.size track
  times <- Grid.newMutable nrows ncols (-1)
  let race !t !pos !dir = do
        Grid.write times pos t
        let next =
              listToMaybe
                [ (p, d)
                | d <- [dir, Pos.turnLeft dir, Pos.turnRight dir],
                  let p = Pos.move d pos,
                  Grid.unsafeIndex track p
                ]
        case next of
          Nothing -> pure ()
          Just (p, d) -> race (t + 1) p d

  race 0 start $
    head
      [ d
      | d <- [North, East, West, South],
        Grid.unsafeIndex track (Pos.move d start)
      ]
  pure times

solve :: Grid VU.Vector Char -> (Int, Int)
solve grid = (part1, part2)
  where
    Pair (Sum part1) (Sum part2) =
      mconcat $
        parMap rseq (\p -> Pair (cheat 2 p) (cheat 20 p)) $
          VU.toList $
            Grid.findPositions (>= 0) times
    (nrows, ncols) = Grid.size grid
    !start = fromJust $ Grid.findPosition (== 'S') grid
    !track = Grid.map (/= '#') grid
    !times = racePath track start
    cheat n p1 =
      let t1 = Grid.unsafeIndex times p1
       in foldRange (max (-n) (1 - row p1)) (min n (nrows - row p1 - 2)) 0 $ \acc1 dr ->
            foldRange
              (max (abs dr - n) (1 - col p1))
              (min (n - abs dr) (ncols - col p1 - 2))
              acc1
              $ \acc dc ->
                let !p2 = Position {row = row p1 + dr, col = col p1 + dc}
                    !t2 = Grid.unsafeIndex times p2
                    save = t1 - t2 - abs dr - abs dc
                 in if (t2 >= 0 && save >= 100) then acc + 1 else acc

foldRange :: Int -> Int -> a -> (a -> Int -> a) -> a
foldRange !start !stop x f = go start x
  where
    go !i !acc
      | i > stop = acc
      | otherwise = go (i + 1) (f acc i)
