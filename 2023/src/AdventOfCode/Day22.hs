module AdventOfCode.Day22 (solution) where

import AdventOfCode.Parser qualified as Parser
import AdventOfCode.Prelude
import Control.DeepSeq (NFData (..))
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.List (findIndices)
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
    { parser = Vector.fromList <$> parseBrick `sepEndBy'` Parser.endOfLine,
      solver = solve1 &&& solve2
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
solve1 bricks = Vector.length bricks - required
  where
    dropped = dropBricks bricks
    supportedOnlyBy = Vector.filter ((== 1) . IntSet.size) (supporters dropped)
    required = IntSet.size $ IntSet.unions $ Vector.toList supportedOnlyBy

solve2 :: Vector Brick -> Int
solve2 bricks =
  sum $ map (disintegrate suspended . IntSet.singleton) [0 .. Vector.length bricks - 1]
  where
    indices = Vector.enumFromN 0 (Vector.length bricks)
    supportedBy = (supporters (dropBricks bricks) Vector.!)
    suspended = Vector.dropWhile (IntSet.null . supportedBy) indices
    disintegrate is falling
      | Vector.null fall = 0
      | otherwise =
          Vector.length fall
            + disintegrate
              (Vector.dropWhile (< Vector.head fall) rest)
              (falling <> fallSet)
      where
        (fall, rest) =
          Vector.partition
            ((`IntSet.isSubsetOf` falling) . supportedBy)
            is
        fallSet = IntSet.fromDistinctAscList . Vector.toList $ fall

dropBrickTo :: Int -> Brick -> Brick
dropBrickTo height Brick {start, end} =
  Brick {start = start {z = height}, end = end {z = z end - z start + height}}

dropBricks :: Vector Brick -> Vector Brick
dropBricks bricks = Vector.fromListN (Vector.length bricks) dropped
  where
    sorted = sortOn (z . start) $ Vector.toList bricks
    dropped = zipWith go [0 ..] sorted
    go i brick = dropBrickTo (highest + 1) brick
      where
        intersections = filter (intersectXY brick) (take i dropped)
        highest = maximum $ 0 : map (z . end) intersections

supporters :: Vector Brick -> Vector IntSet
supporters bricks =
  Vector.fromListN (Vector.length bricks) $
    zipWith supportedBy [0 ..] sorted
  where
    sorted = sortOn (z . start) $ Vector.toList bricks
    supportedBy i brick =
      IntSet.fromDistinctAscList $
        findIndices (isSupporter brick) (take i sorted)
    isSupporter brick b =
      z (end b) + 1 == z (start brick) && intersectXY brick b

intersectXY :: Brick -> Brick -> Bool
intersectXY
  Brick {start = start1, end = end1}
  Brick {start = start2, end = end2} =
    x start1 <= x end2
      && x start2 <= x end1
      && y start1 <= y end2
      && y start2 <= y end1
