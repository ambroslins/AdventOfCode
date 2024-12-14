module AdventOfCode.Day12 (solution) where

import AdventOfCode.Grid (Grid)
import AdventOfCode.Grid qualified as Grid
import AdventOfCode.Position qualified as Pos
import AdventOfCode.Prelude
import Control.Monad (foldM, forM_, when)
import Control.Monad.ST.Strict (runST)
import Data.STRef.Strict (newSTRef, readSTRef, writeSTRef)
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as MVU

solution :: Solution
solution =
  Solution
    { parser = Grid.parse,
      solver = solve
    }

solve :: Grid VU.Vector Char -> (Int, Int)
solve labels = runST $ do
  regions <- Grid.newMutable @VU.Vector nrows ncols (-1 :: Int)
  areas <- MVU.replicate maxRegions (0 :: Int)
  perimeters <- MVU.replicate maxRegions (0 :: Int)
  sides <- MVU.replicate maxRegions (0 :: Int)
  nextRegion <- newSTRef (0 :: Int)

  let mark !area !perimeter !region !label !pos = do
        r <- Grid.read regions pos
        if r >= 0
          then pure $ Pair area perimeter
          else do
            Grid.write regions pos region
            let move (Pair a p) dir =
                  let !next = Pos.move dir pos
                   in if Grid.index labels next == Just label
                        then mark a p region label next
                        else pure $ Pair a (p + 1)
            foldM move (Pair (area + 1) perimeter) [North, South, East, West]
      newRegion !label !pos = do
        r <- Grid.read regions pos
        if r >= 0
          then pure ()
          else do
            region <- readSTRef nextRegion
            writeSTRef nextRegion (region + 1)
            Pair area perimeter <- mark 0 0 region label pos
            MVU.write areas region area
            MVU.write perimeters region perimeter

  forM_ [0 .. nrows - 1] $ \row -> forM_ [0 .. ncols - 1] $ \col ->
    let pos = Position {row, col}
     in newRegion (Grid.unsafeIndex labels pos) pos

  forM_ [0 .. nrows] $ \row -> forM_ [0 .. ncols] $ \col -> do
    let reg pos = fromMaybe (-1) <$> Grid.readMaybe regions pos
    a <- reg $ Position {row = row - 1, col = col - 1}
    b <- reg $ Position {row = row - 1, col = col}
    c <- reg $ Position {row = row, col = col}
    d <- reg $ Position {row = row, col = col - 1}

    when (isCorner a b c d) $ MVU.modify sides (+ 1) a
    when (isCorner b c d a) $ MVU.modify sides (+ 1) b
    when (isCorner c d a b) $ MVU.modify sides (+ 1) c
    when (isCorner d a b c) $ MVU.modify sides (+ 1) d

  as <- VU.unsafeFreeze areas
  ps <- VU.unsafeFreeze perimeters
  ss <- VU.unsafeFreeze sides
  let !price1 = VU.sum $ VU.zipWith (*) as ps
      !price2 = VU.sum $ VU.zipWith (*) as ss

  pure (price1, price2)
  where
    (nrows, ncols) = Grid.size labels
    maxRegions = 1000

isCorner :: Int -> Int -> Int -> Int -> Bool
isCorner !a !b !c !d =
  a >= 0 && (a /= b && a /= d || a == b && a == d && a /= c)
