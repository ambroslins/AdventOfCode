module AdventOfCode.Day14 (solution) where

import AdventOfCode.Grid (Grid)
import AdventOfCode.Grid qualified as Grid
import AdventOfCode.Prelude
import Data.HashMap.Strict qualified as HashMap
import Data.Vector.Unboxed (Vector)
import Data.Vector.Unboxed qualified as Vector
import Data.Vector.Unboxed.Mutable qualified as MVector
import Prelude hiding (cycle)

solution :: Solution
solution =
  Solution
    { parser = Grid.parse,
      part1 = solve1,
      part2 = solve2
    }

solve1 :: Grid Vector Char -> Int
solve1 = sum . map (load . roll) . Grid.cols

solve2 :: Grid Vector Char -> Int
solve2 grid =
  case findLoop $ map (Grid.findPositions (== 'O')) cycles of
    Nothing -> error "No loop found"
    Just (n, m) ->
      let k = (1_000_000_000 - m) `mod` n
       in totalLoad $ cycles !! (k + m)
  where
    cycles = iterate cycle grid

load :: Vector Char -> Int
load v = Vector.sum $ Vector.imap f v
  where
    l = Vector.length v
    f i r = if r == 'O' then l - i else 0

totalLoad :: Grid Vector Char -> Int
totalLoad = sum . map load . Grid.cols

roll :: Vector Char -> Vector Char
roll v = Vector.create $ do
  w <- MVector.replicate (Vector.length v) '.'
  let go j i = \case
        'O' -> MVector.write w j 'O' $> (j + 1)
        '#' -> MVector.write w i '#' $> (i + 1)
        _ -> pure j
  Vector.ifoldM'_ go 0 v
  pure w

cycle :: Grid Vector Char -> Grid Vector Char
cycle = east . south . west . north
  where
    reverseRoll = Vector.reverse . roll . Vector.reverse
    north = Grid.fromCols . map roll . Grid.cols
    west = Grid.fromRows . map roll . Grid.rows
    south = Grid.fromCols . map reverseRoll . Grid.cols
    east = Grid.fromRows . map reverseRoll . Grid.rows

findLoop :: (Hashable a) => [a] -> Maybe (Int, Int)
findLoop = go HashMap.empty 0
  where
    go seen !i = \case
      [] -> Nothing
      x : xs ->
        case HashMap.lookup x seen of
          Just j -> Just (i - j, j)
          Nothing -> go (HashMap.insert x i seen) (i + 1) xs
