module AdventOfCode.Day11 (solution) where

import AdventOfCode.Parser qualified as Parser
import AdventOfCode.Prelude
import Data.ByteString.Char8 qualified as BS
import Data.IntSet qualified as IntSet
import Data.List (tails)

solution :: Solution
solution =
  Solution
    { parser = parseGalaxies <$> Parser.lines,
      solver = solve 2 &&& solve 1000000
    }

parseGalaxies :: [ByteString] -> [Position]
parseGalaxies ls = do
  (row, line) <- zip [0 ..] ls
  col <- BS.findIndices (== '#') line
  pure Position {row, col}

expand :: Int -> [Position] -> [Position]
expand factor galaxies = map shift galaxies
  where
    cols = IntSet.fromList $ map col galaxies
    rows = IntSet.fromList $ map row galaxies
    shift Position {row, col} =
      let dr = IntSet.size $ fst $ IntSet.split row rows
          dc = IntSet.size $ fst $ IntSet.split col cols
       in Position
            { row = factor * (row - dr) + dr,
              col = factor * (col - dc) + dc
            }

solve :: Int -> [Position] -> Int
solve factor galaxies = sum $ do
  p1 : ps <- tails $ expand factor galaxies
  p2 <- ps
  let dr = abs $ row p1 - row p2
      dc = abs $ col p1 - col p2
  pure $ dr + dc
