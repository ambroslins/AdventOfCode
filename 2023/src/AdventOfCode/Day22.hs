module AdventOfCode.Day22 (solution) where

import AdventOfCode.Parser qualified as Parser
import AdventOfCode.Prelude
import Control.DeepSeq (NFData (..))
import Control.Parallel.Strategies (parMap, rseq)
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.List (findIndices)
import Data.Vector (Vector, (!))
import Data.Vector qualified as Vector
import Data.Vector.Unboxed qualified as Unboxed

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
      solver = solve
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

solve :: Vector Brick -> (Int, Int)
solve bricks =
  ( Vector.length bricks - IntSet.size required,
    sum $
      parMap
        rseq
        (disintegrate 0 suspended . IntSet.singleton)
        (Unboxed.toList indices)
  )
  where
    supportedBy = supporters (dropBricks bricks)
    required =
      Vector.foldl' (<>) IntSet.empty $
        Vector.filter ((== 1) . IntSet.size) supportedBy
    indices = Unboxed.enumFromN 0 (Vector.length bricks)
    suspended = Unboxed.dropWhile (\i -> IntSet.null (supportedBy ! i)) indices
    disintegrate !n !is !falling
      | Unboxed.null fall = n
      | otherwise =
          disintegrate
            (Unboxed.length fall + n)
            (Unboxed.dropWhile (< Unboxed.head fall) rest)
            (falling <> fallSet)
      where
        (fall, rest) =
          Unboxed.partition
            (\i -> supportedBy ! i `IntSet.isSubsetOf` falling)
            is
        fallSet = IntSet.fromDistinctAscList . Unboxed.toList $ fall

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
