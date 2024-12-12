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
  areas <- MVU.replicate (nrows * ncols) (0 :: Int)
  perimeters <- MVU.replicate (nrows * ncols) (0 :: Int)
  sides <- MVU.replicate (nrows * ncols) (0 :: Int)
  nextRegion <- newSTRef (0 :: Int)

  let mark !region !label !pos = do
        r <- Grid.read regions pos
        if r >= 0
          then pure ()
          else do
            Grid.write regions pos region
            MVU.modify areas (+ 1) region
            let move s dir =
                  let !p = Pos.move dir pos
                   in if Grid.index labels p == Just label
                        then mark region label p $> s
                        else pure (s + 1)
            s <- foldM move 0 [North, South, East, West]
            MVU.modify perimeters (+ s) region
      newRegion !label !pos = do
        r <- Grid.read regions pos
        if r >= 0
          then pure ()
          else do
            region <- readSTRef nextRegion
            writeSTRef nextRegion (region + 1)
            mark region label pos

  VU.forM_ (Grid.findPositions (const True) labels) $ \pos ->
    newRegion (Grid.unsafeIndex labels pos) pos

  forM_ [0 .. nrows] $ \row -> forM_ [0 .. ncols] $ \col -> do
    let reg pos = fromMaybe (-1) <$> Grid.readMaybe regions pos
    a <- reg $ Position {row = row - 1, col = col - 1}
    b <- reg $ Position {row = row - 1, col = col}
    c <- reg $ Position {row = row, col = col - 1}
    d <- reg $ Position {row = row, col = col}

    when (a >= 0 && (a /= b && a /= c || a == b && a == c && a /= d)) $
      MVU.modify sides (+ 1) a
    when (b >= 0 && (b /= d && b /= a || b == a && b == d && b /= c)) $
      MVU.modify sides (+ 1) b
    when (c >= 0 && (c /= d && c /= a || c == a && c == d && c /= b)) $
      MVU.modify sides (+ 1) c
    when (d >= 0 && (d /= b && d /= c || d == b && d == c && d /= a)) $
      MVU.modify sides (+ 1) d

  as <- VU.unsafeFreeze areas
  ps <- VU.unsafeFreeze perimeters
  ss <- VU.unsafeFreeze sides
  let !price1 = VU.sum $ VU.zipWith (*) as ps
      !price2 = VU.sum $ VU.zipWith (*) as ss

  pure (price1, price2)
  where
    (nrows, ncols) = Grid.size labels

{-
1 : 4  => 4
3 : 8  => 6
4 : 8 => 4
5 : 12  => 6
14 : 28 => 22
10 : 18 => 12
11 : 20 => 12
12 : 18 => 10
13 : 20 => 10
13 : 18 => 8
14 : 22 => 16
 -}
