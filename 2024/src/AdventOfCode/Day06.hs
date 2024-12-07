{-# LANGUAGE MultiWayIf #-}

module AdventOfCode.Day06 (solution) where

import AdventOfCode.Grid (Grid)
import AdventOfCode.Grid qualified as Grid
import AdventOfCode.Position qualified as Pos
import AdventOfCode.Prelude
import Control.Monad (when)
import Control.Monad.ST.Strict (runST)
import Data.Bits (setBit, testBit)
import Data.Vector.Unboxed (Vector)
import Data.Vector.Unboxed.Mutable qualified as MVector

solution :: Solution
solution =
  Solution
    { parser = Grid.parse,
      solver = solve
    }

solve :: Grid Vector Char -> (Int, Int)
solve grid = runST $ do
  obstacles <- Grid.unsafeThaw $ Grid.map (== '#') grid
  obstructions <- Grid.newMutable @Vector nrows ncols False

  visited1 <- Grid.newMutable @Vector nrows ncols (0 :: Word)
  visited2 <- Grid.newMutable @Vector nrows ncols (0 :: Word)

  let doesLoop !pos !dir = do
        bs <- Grid.read visited2 pos
        if bs `testBit` (fromEnum dir)
          then pure True
          else do
            Grid.write visited2 pos (bs `setBit` fromEnum dir)
            let next = Pos.move dir pos
            Grid.readMaybe obstacles next >>= \case
              Nothing -> pure False
              Just True -> doesLoop pos (Pos.turnRight dir)
              Just False -> doesLoop next dir

  let walk !count1 !count2 !pos !dir = do
        bs <- Grid.read visited1 pos
        let c1 = if bs == 0 then count1 + 1 else count1
        when (bs `testBit` fromEnum dir) $ error "solve: unexpected loop"
        Grid.write visited1 pos (bs `setBit` fromEnum dir)
        let next = Pos.move dir pos
        Grid.readMaybe obstacles next >>= \case
          Nothing -> pure (c1, count2)
          Just True -> walk c1 count2 pos (Pos.turnRight dir)
          Just False -> do
            c2 <-
              Grid.read obstructions next >>= \case
                True -> pure count2 -- already tested
                False -> do
                  Grid.write obstructions next True
                  MVector.copy (Grid.cells visited2) (Grid.cells visited1)
                  Grid.write obstacles next True
                  loops <- doesLoop pos (Pos.turnRight dir)
                  Grid.write obstacles next False
                  pure $ if loops then count2 + 1 else count2
            walk c1 c2 next dir

  walk 0 0 start North
  where
    start =
      fromMaybe (error "solve: no guard") $
        Grid.findPosition (== '^') grid
    (nrows, ncols) = Grid.size grid
