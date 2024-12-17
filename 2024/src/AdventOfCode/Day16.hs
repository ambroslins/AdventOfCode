module AdventOfCode.Day16 (solution) where

import AdventOfCode.Grid (Grid)
import AdventOfCode.Grid qualified as Grid
import AdventOfCode.Position qualified as Pos
import AdventOfCode.Prelude
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

solve :: Grid VU.Vector Char -> (Int, Int)
solve grid = runST $ do
  costs <- MVU.replicate (nrows * ncols * 4) (nrows * ncols * 1000 :: Int)

  let walk queue = case Deque.uncons queue of
        Nothing -> pure ()
        Just ((pos, dir, cost), q) -> do
          let i = index pos * 4 + fromEnum dir
          c <- MVU.read costs i
          if c <= cost
            then walk q
            else do
              MVU.write costs i cost
              walk (q <> fromList (next pos dir cost))

  walk $ fromList [(start, East, 0)]

  cs <- VU.unsafeFreeze costs
  let (d1, c1) = VU.minimumOn snd $ VU.indexed $ VU.slice (index end * 4) 4 $ cs

  sit <- Grid.newMutable nrows ncols (0 :: Int)

  let walkBack = \case
        [] -> pure ()
        ((pos, dir, cost) : s) -> do
          Grid.write sit pos 1
          let previous =
                [ (p, dir, c)
                | let p = Pos.move (Pos.invert dir) pos,
                  let c = cs VU.! (index p * 4 + fromEnum dir),
                  c == cost - 1
                ]
                  ++ [ (pos, d, c)
                     | d <- [Pos.turnRight dir, Pos.turnLeft dir],
                       let c = cs VU.! (index pos * 4 + fromEnum d),
                       c == cost - 1000
                     ]
           in walkBack (previous <> s)

  walkBack [(end, toEnum d1, c1)]

  c2 <- MVU.foldl' (+) 0 (Grid.cells sit)

  pure (c1, c2)
  where
    (nrows, ncols) = Grid.size grid
    start = fromJust $ Grid.findPosition (== 'S') grid
    end = fromJust $ Grid.findPosition (== 'E') grid
    next !pos !dir !cost
      | pos == end = []
      | otherwise =
          [(p, dir, cost + 1) | let p = Pos.move dir pos, Grid.unsafeIndex grid p /= '#']
            ++ [ (pos, d, cost + 1000)
               | d <- [Pos.turnLeft dir, Pos.turnRight dir],
                 Grid.unsafeIndex grid (Pos.move d pos) /= '#'
               ]
    index Position {row, col} = (row * ncols + col)
