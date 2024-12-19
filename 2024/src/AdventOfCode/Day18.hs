module AdventOfCode.Day18 (solution) where

import AdventOfCode.BucketQueue qualified as BQ
import AdventOfCode.Grid (Grid)
import AdventOfCode.Grid qualified as Grid
import AdventOfCode.Parser qualified as Parser
import AdventOfCode.Position qualified as Pos
import AdventOfCode.Prelude
import Control.Monad (forM_)
import Control.Monad.ST.Strict (runST)
import Data.Bits (shiftR)
import Data.Vector.Unboxed qualified as VU
import Deque.Strict qualified as Deque
import GHC.IsList (fromList)

size :: Int
size = 71

solution :: Solution
solution =
  Solution
    { parser = parsePosition `sepEndBy` Parser.endOfLine,
      solver = solve
    }

parsePosition :: Parser Position
parsePosition = do
  col <- Parser.decimal
  Parser.symbol ","
  row <- Parser.decimal
  pure $ Position {row, col}

build :: [Position] -> Grid VU.Vector Int
build ps = Grid.create $ do
  grid <- Grid.newMutable size size 0
  forM_ (zip [1 ..] ps) $ \(i, p) -> Grid.write grid p i
  pure grid

solve :: [Position] -> (Int, Int)
solve ps =
  ( fromJust $ findPath 1024 corrupted,
    binarySearch
      cutOff
      1024
      (VU.maximum $ Grid.cells corrupted)
  )
  where
    corrupted = build ps
    cutOff t = isNothing $ findPath t corrupted

findPath :: Int -> Grid VU.Vector Int -> Maybe Int
findPath !t corrupted = runST $ do
  seen <- Grid.newMutable @VU.Vector nrows ncols False
  let walk queue = case Deque.uncons queue of
        Nothing -> pure Nothing
        Just ((steps, pos), q)
          | pos == end -> pure $! Just steps
          | otherwise ->
              Grid.read seen pos >>= \case
                True -> walk q
                False -> do
                  Grid.write seen pos True
                  walk (q <> fromList (nexts pos steps))
  walk $ fromList [(0, start)]
  where
    (nrows, ncols) = Grid.size corrupted
    start = Position {row = 0, col = 0}
    end = Position {row = nrows - 1, col = ncols - 1}
    nexts !pos !steps =
      [ (steps + 1, p)
      | dir <- [North, East, South, West],
        let p = Pos.move dir pos,
        maybe False (\b -> b == 0 || b > t) $ Grid.index corrupted p
      ]

binarySearch :: (Int -> Bool) -> Int -> Int -> Int
binarySearch p = go
  where
    go !low !high
      | low > high = high + 1
      | p mid = go low (mid - 1)
      | otherwise = go (mid + 1) high
      where
        mid = low + (high - low) `shiftR` 1
