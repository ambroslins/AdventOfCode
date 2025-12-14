module AdventOfCode.Day09 (solution) where

import AdventOfCode.Parser qualified as Parser
import AdventOfCode.Prelude
import Data.ByteString qualified as BS
import Data.List qualified as List
import Prelude hiding (lines)

solution :: Solution
solution =
  Solution
    { parser = parsePoint `sepEndBy'` Parser.endOfLine,
      solver = solve
    }

data Point = Point !Int !Int
  deriving (Show, Eq, Ord)

data Rectangle = Rectangle {area :: !Int, from :: !Point, to :: !Point}
  deriving (Show, Eq, Ord)

data Line = Line !Int !Point !Point
  deriving (Show, Eq, Ord)

parsePoint :: Parser Point
parsePoint = do
  x <- Parser.decimal
  _ <- Parser.char ','
  y <- Parser.decimal
  pure $! Point x y

makeRectangle :: Point -> Point -> Rectangle
makeRectangle (Point x1 y1) (Point x2 y2) =
  Rectangle
    ((abs (x2 - x1) + 1) * (abs (y2 - y1) + 1))
    (Point (min x1 x2) (min y1 y2))
    (Point (max x1 x2) (max y1 y2))

makeLine :: Point -> Point -> Line
makeLine (Point x1 y1) (Point x2 y2) =
  Line
    (max (abs (x2 - x1) + 1) (abs (y2 - y1) + 1))
    (Point (min x1 x2) (min y1 y2))
    (Point (max x1 x2) (max y1 y2))

solve :: [Point] -> (Int, Int)
solve points =
  ( area $ head rectangles,
    area $ head $ filter (\r -> not (any (interesects r) lines)) rectangles
  )
  where
    rectangles =
      List.sortBy (flip compare) $
        [makeRectangle p1 p2 | p1 : ps <- List.tails points, p2 <- ps]
    lines =
      List.sortBy (flip compare) $
        zipWith makeLine points (drop 1 points)
    interesects (Rectangle _ (Point x1 y1) (Point x2 y2)) (Line _ (Point x3 y3) (Point x4 y4)) =
      x3 < x2 && y3 < y2 && x4 > x1 && y4 > y1
