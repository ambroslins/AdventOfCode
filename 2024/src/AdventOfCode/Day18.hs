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
  ( fromJust $ astar 1024 corrupted,
    binarySearch cutOff 1024 (length ps)
  )
  where
    corrupted = build ps
    cutOff t = isNothing $ astar t corrupted

astar :: Int -> Grid VU.Vector Int -> Maybe Int
astar !t corrupted = runST $ do
  seen <- Grid.newMutable @VU.Vector nrows ncols False
  queue <- BQ.fromList (size * 4) [(0, (start, 0))]
  let walk =
        BQ.dequeue queue >>= \case
          Nothing -> pure Nothing
          Just (_, (pos, steps))
            | pos == end -> pure $! Just steps
            | otherwise ->
                Grid.read seen pos >>= \case
                  True -> walk
                  False -> do
                    Grid.write seen pos True
                    BQ.enqueueList queue (nexts pos steps)
                    walk
  walk
  where
    (nrows, ncols) = Grid.size corrupted
    start = Position {row = 0, col = 0}
    end = Position {row = nrows - 1, col = ncols - 1}
    nexts !pos !steps =
      [ (steps + 1 + h, (p, steps + 1))
      | dir <- [North, East, South, West],
        let p = Pos.move dir pos,
        let h = (row end - row p) + (col end - col p),
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
