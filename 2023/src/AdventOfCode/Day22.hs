module AdventOfCode.Day22 (solution) where

import AdventOfCode.Parser qualified as Parser
import AdventOfCode.Prelude
import Control.DeepSeq (NFData (..))
import Data.IntSet qualified as IntSet
import Data.Ord (comparing)
import Data.Vector (Vector)
import Data.Vector qualified as Vector

data Point = Point {x, y, z :: !Int}
  deriving (Eq, Show)

instance NFData Point where
  rnf p = p `seq` ()

data Brick = Brick {start, end :: !Point}
  deriving (Eq, Show)

instance NFData Brick where
  rnf b = b `seq` ()

solution :: Solution
solution =
  Solution
    { parser = Vector.fromList <$> parseBrick `sepEndBy` Parser.endOfLine,
      part1 = solve1,
      part2 = all (\Brick {start, end} -> x start <= x end && y start <= y end && z start <= z end)
    }

parsePoint :: Parser Point
parsePoint = do
  x <- Parser.decimal <* Parser.char ','
  y <- Parser.decimal <* Parser.char ','
  z <- Parser.decimal
  pure Point {x, y, z}

parseBrick :: Parser Brick
parseBrick = do
  start <- parsePoint
  _ <- Parser.char '~'
  end <- parsePoint
  pure Brick {start, end}

solve1 :: Vector Brick -> Int
solve1 bricks = Vector.length dropped - required
  where
    dropped = dropBricks bricks
    supportedOnlyBy i brick =
      let supporters =
            Vector.findIndices
              (\b -> z (end b) + 1 == z (start brick) && intersectXY brick b)
              $ Vector.take i dropped
       in if Vector.length supporters == 1
            then Just $ Vector.head supporters
            else Nothing
    required =
      IntSet.size . IntSet.fromList . Vector.toList $
        Vector.imapMaybe supportedOnlyBy dropped

dropBrickTo :: Int -> Brick -> Brick
dropBrickTo height Brick {start, end} =
  Brick {start = start {z = height}, end = end {z = z end - z start + height}}

dropBricks :: Vector Brick -> Vector Brick
dropBricks bricks = Vector.fromListN (Vector.length bricks) dropped
  where
    sorted = sortBy (comparing (z . start)) $ Vector.toList bricks
    dropped = zipWith go [0 ..] sorted
    go i brick = dropBrickTo (highest + 1) brick
      where
        intersecting = filter (intersectXY brick) (take i dropped)
        highest = maximum $ 0 : map (z . end) intersecting

intersectXY :: Brick -> Brick -> Bool
intersectXY
  Brick {start = start1, end = end1}
  Brick {start = start2, end = end2} =
    x start1 <= x end2
      && x start2 <= x end1
      && y start1 <= y end2
      && y start2 <= y end1
