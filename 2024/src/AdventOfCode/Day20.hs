module AdventOfCode.Day20 (solution) where

import AdventOfCode.Grid (Grid)
import AdventOfCode.Grid qualified as Grid
import AdventOfCode.Position qualified as Pos
import AdventOfCode.Prelude
import Control.Parallel.Strategies (dot, rdeepseq, rpar, runEval)
import Data.Int (Int32)
import Data.Vector.Unboxed qualified as VU

solution :: Solution
solution =
  Solution
    { parser = Grid.parse,
      solver = solve
    }

racePath :: Grid VU.Vector Bool -> Position -> VU.Vector Position
racePath track start =
  VU.unfoldrExactN
    (VU.foldl' (\acc b -> if b then acc + 1 else acc) 0 (Grid.cells track))
    go
    (Pair start startDir)
  where
    startDir =
      head
        [ d
        | d <- [North, East, West, South],
          Grid.unsafeIndex track (Pos.move d start)
        ]
    go (Pair pos dir) =
      let next =
            head
              [ (p, d)
              | d <- [dir, Pos.turnLeft dir, Pos.turnRight dir],
                let p = Pos.move d pos,
                Grid.unsafeIndex track p
              ]
       in (pos, uncurry Pair next)

solve :: Grid VU.Vector Char -> (Int32, Int32)
solve grid = (VU.sum part1, VU.sum part2)
  where
    (part1, part2) =
      VU.unzip $
        runEval $
          VU.generateM
            (VU.length path - 100)
            (\t -> (rpar `dot` rdeepseq) (cheat 2 t, cheat 20 t))
    !start = fromJust $ Grid.findPosition (== 'S') grid
    !track = Grid.map (/= '#') grid
    !path = racePath track start
    cheat !n !t1 =
      let !p1 = path VU.! t1
          go !acc !t2
            | t2 >= VU.length path = acc
            | d > n = go acc (t2 + d - n)
            | otherwise = go (if save >= 100 then acc + 1 else acc) (t2 + 1)
            where
              p2 = path VU.! t2
              d = distance p1 p2
              save = t2 - t1 - d
       in go 0 (t1 + 100)

distance :: Position -> Position -> Int
distance p1 p2 = abs (row p2 - row p1) + abs (col p2 - col p1)
