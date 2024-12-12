module AdventOfCode.Day12 (solution) where

import AdventOfCode.Grid (Grid)
import AdventOfCode.Grid qualified as Grid
import AdventOfCode.Position qualified as Pos
import AdventOfCode.Prelude
import Control.Monad (forM_)
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
  nextRegion <- newSTRef (0 :: Int)

  let mark !region !label !pos = do
        r <- Grid.read regions pos
        if r >= 0
          then pure ()
          else do
            Grid.write regions pos region
            MVU.modify areas (+ 1) region
            forM_ [North, South, East, West] $ \dir ->
              let !p = Pos.move dir pos
               in if Grid.index labels p == Just label
                    then mark region label p
                    else MVU.modify perimeters (+ 1) region
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

  as <- VU.unsafeFreeze areas
  ps <- VU.unsafeFreeze perimeters
  let price = VU.sum $ VU.zipWith (*) as ps

  pure (price, 0)
  where
    (nrows, ncols) = Grid.size labels
