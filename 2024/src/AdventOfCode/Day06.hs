{-# LANGUAGE MultiWayIf #-}

module AdventOfCode.Day06 (solution) where

import AdventOfCode.Grid (Grid)
import AdventOfCode.Grid qualified as Grid
import AdventOfCode.Position qualified as Pos
import AdventOfCode.Prelude
import Control.Monad.ST.Strict (runST)
import Data.Bits (setBit, testBit)
import Data.Vector.Unboxed (Vector)
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as MVU
import Data.Word (Word8)

solution :: Solution
solution =
  Solution
    { parser = Grid.parse,
      solver = solve
    }

solve :: Grid Vector Char -> (Int, Int)
solve grid = runST $ do
  obstructions <- Grid.newMutable @Vector nrows ncols False

  visited1 <- Grid.newMutable @Vector nrows ncols (0 :: Word8)
  visited2 <- Grid.newMutable @Vector nrows ncols (0 :: Word8)

  let doesLoop !obs !pos !dir = do
        bs <- Grid.read visited2 pos
        if bs `testBit` (fromEnum dir)
          then pure True
          else do
            Grid.write visited2 pos (bs `setBit` fromEnum dir)
            let step = fromIntegral $ case dir of
                  North -> Grid.unsafeIndex northSteps pos
                  East -> Grid.unsafeIndex eastSteps pos
                  South -> Grid.unsafeIndex southSteps pos
                  West -> Grid.unsafeIndex westSteps pos
                stepsToObs = case dir of
                  North | col pos == col obs -> row pos - row obs
                  East | row pos == row obs -> col obs - col pos
                  South | col pos == col obs -> row obs - row pos
                  West | row pos == row obs -> col pos - col obs
                  _ -> 0
                actualSteps
                  | stepsToObs <= 0 = step
                  | otherwise = min step stepsToObs
                next = Pos.moveN dir (actualSteps - 1) pos

            if Grid.inside next obstacles
              then doesLoop obs next (Pos.turnRight dir)
              else pure False

  let walk !count1 !count2 !pos !dir = do
        bs <- Grid.read visited1 pos
        let c1 = if bs == 0 then count1 + 1 else count1
        Grid.write visited1 pos (bs `setBit` fromEnum dir)
        let next = Pos.move dir pos
        case Grid.index obstacles next of
          Nothing -> pure (c1, count2)
          Just True -> walk c1 count2 pos (Pos.turnRight dir)
          Just False -> do
            c2 <-
              Grid.read obstructions next >>= \case
                True -> pure count2 -- already tested
                False -> do
                  Grid.write obstructions next True
                  MVU.copy (Grid.cells visited2) (Grid.cells visited1)
                  loops <- doesLoop next pos (Pos.turnRight dir)
                  pure $ if loops then count2 + 1 else count2
            walk c1 c2 next dir

  walk 0 0 start North
  where
    !start =
      fromMaybe (error "solve: no guard") $
        Grid.findPosition (== '^') grid
    (nrows, ncols) = Grid.size grid
    !obstacles = Grid.map (== '#') grid
    steps !n !o = if o then 0 else n + 1 :: Word8
    !northSteps =
      Grid.fromCols $
        map (VU.postscanl' steps 1) $
          Grid.cols obstacles
    !southSteps =
      Grid.fromCols $
        map (VU.postscanr' (flip steps) 1) $
          Grid.cols obstacles
    !eastSteps =
      Grid.fromRows $
        map (VU.postscanr' (flip steps) 1) $
          Grid.rows obstacles
    !westSteps =
      Grid.fromRows $
        map (VU.postscanl' steps 1) $
          Grid.rows obstacles
