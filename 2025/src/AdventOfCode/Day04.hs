{-# HLINT ignore "Use for_" #-}
{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module AdventOfCode.Day04 (solution) where

import AdventOfCode.Grid qualified as Grid
import AdventOfCode.Prelude
import Control.Monad (forM_, unless, when)
import Data.Int (Int8)
import Data.Vector.Unboxed (Vector)
import Data.Vector.Unboxed qualified as Vector

solution :: Solution
solution =
  Solution
    { parser = Grid.parse,
      solver = solve
    }

solve :: Grid.Grid Vector Char -> (Int, Int)
solve grid = (solve1 ns, solve2 ns)
  where
    ns = neighbours grid

solve1 :: Grid.Grid Vector Int8 -> Int
solve1 = countVector (< 4) . Grid.cells

countVector :: (Vector.Unbox a) => (a -> Bool) -> Vector a -> Int
countVector p = Vector.foldl' (\acc x -> if p x then acc + 1 else acc) 0

solve2 :: Grid.Grid Vector Int8 -> Int
solve2 grid = countVector (< 0) $ Grid.cells $ Grid.create $ do
  g <- Grid.thaw grid
  let go !pos = do
        n <- Grid.read g pos
        when (n >= 0 && n < 4) $ do
          Grid.write g pos (-1)
          for (-1) 1 $ \dr ->
            for (-1) 1 $ \dc ->
              unless (dr == 0 && dc == 0) $ do
                let !p = Position {row = row pos + dr, col = col pos + dc}
                when (Grid.inside p grid) $ do
                  Grid.modify g (\x -> x - 1) p
                  go p

  Vector.mapM_ go $ Grid.findPositions (< 4) grid
  pure g

neighbours :: Grid.Grid Vector Char -> Grid.Grid Vector Int8
neighbours grid = Grid.create $ do
  g <- Grid.newMutable nrows ncols 127
  for 0 (ncols - 1) $ \c -> do
    for 0 (nrows - 1) $ \r -> do
      let p = Position {row = r, col = c}
      when (Grid.unsafeIndex grid p == '@') $ do
        let !n =
              count (== '@') $
                mapMaybe
                  (Grid.index grid)
                  [ Position {row = r + dr, col = c + dc}
                  | dc <- [-1, 0, 1],
                    dr <- [-1, 0, 1],
                    dc /= 0 || dr /= 0
                  ]
        Grid.write g p (fromIntegral n)
  pure g
  where
    (nrows, ncols) = Grid.size grid
