{-# LANGUAGE OverloadedLists #-}

module AdventOfCode.Day24 (solution) where

import AdventOfCode.Parser qualified as Parser
import AdventOfCode.Prelude
import Control.DeepSeq (NFData (rnf))
import Data.List (tails)
import Data.Vector.Storable qualified as Vector
import Numeric.LinearAlgebra (Matrix, R, Vector, assoc, fromRows, norm_2, rows, toRows, (!), (<\>))

data Vec = Vec {x, y, z :: !Double}
  deriving (Eq, Show)

data Hailstone = Hailstone {position, velocity :: !Vec}
  deriving (Eq, Show)

instance NFData Vec where
  rnf v = v `seq` ()

instance NFData Hailstone where
  rnf h = h `seq` ()

solution :: Solution
solution =
  Solution
    { parser = parseHailstone `sepEndBy'` Parser.endOfLine,
      part1 = solve1,
      part2 = solve2
    }

parseVec :: Parser Vec
parseVec =
  Vec
    <$> (Parser.double <* sep)
    <*> (Parser.double <* sep)
    <*> Parser.double
  where
    sep = Parser.lexeme $ Parser.char ','

parseHailstone :: Parser Hailstone
parseHailstone = Hailstone <$> (parseVec <* Parser.symbol " @") <*> parseVec

intersectionXY :: Hailstone -> Hailstone -> Maybe Vec
intersectionXY
  (Hailstone (Vec x1 y1 _) (Vec dx1 dy1 _))
  (Hailstone (Vec x2 y2 _) (Vec dx2 dy2 _))
    | det /= 0, t1 >= 0, t2 >= 0 = Just $ Vec {x, y, z = 0}
    | otherwise = Nothing
    where
      det = dx1 * dy2 - dx2 * dy1
      t1 = (dx2 * (y1 - y2) - dy2 * (x1 - x2)) / det
      t2 = (dx1 * (y1 - y2) - dy1 * (x1 - x2)) / det
      x = x1 + t1 * dx1
      y = y1 + t1 * dy1

inside :: Vec -> Bool
inside (Vec x y _) = x >= low && x <= high && y >= low && y <= high
  where
    low = 200000000000000
    high = 400000000000000

solve1 :: [Hailstone] -> Int
solve1 hailstones = count inside $ do
  h1 : hs <- tails hailstones
  h2 <- hs
  maybeToList $ intersectionXY h1 h2

solve2 :: [Hailstone] -> Int
solve2 hailstones = round $ Vector.sum $ Vector.take 3 $ newton f j x0
  where
    x0 = Vector.replicate (6 + rows hs) 0
    f = differences hs
    j = jacobian hs
    toVector Hailstone {position = p, velocity = v} = [x p, y p, z p, x v, y v, z v]
    hs = fromRows $ map toVector $ take 5 hailstones

newton ::
  (Vector R -> Vector R) ->
  (Vector R -> Matrix R) ->
  Vector R ->
  Vector R
newton f j = go 0
  where
    go :: Int -> Vector R -> Vector R
    go i x
      | i >= 100 = x
      | norm_2 fx < 1.0 = xn
      | otherwise = go (i + 1) xn
      where
        fx = f x
        xn = x - j x <\> fx

differences :: Matrix R -> Vector R -> Vector R
differences hailstones xs = Vector.generate (3 * rows hailstones) f
  where
    f i = x - xi + ti * (dx - dxi)
      where
        (j, k) = i `divMod` 3
        x = xs ! k
        dx = xs ! (k + 3)
        ti = xs ! (j + 6)
        hailstone = hailstones ! j
        xi = hailstone ! k
        dxi = hailstone ! (k + 3)

jacobian :: Matrix R -> Vector R -> Matrix R
jacobian hailstones xs = assoc (3 * rows hailstones, Vector.length xs) 0 $ do
  i <- [0 .. rows hailstones - 1]
  j <- [0, 1, 2]
  let dx = xs ! (j + 3)
      dxi = hailstones ! i ! (j + 3)
      ti = xs ! (i + 6)
  [ ((i * 3 + j, j), 1.0),
    ((i * 3 + j, j + 3), ti),
    ((i * 3 + j, i + 6), dx - dxi)
    ]
