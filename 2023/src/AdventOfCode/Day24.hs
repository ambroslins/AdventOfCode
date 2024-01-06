{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module AdventOfCode.Day24 (solution) where

import AdventOfCode.Parser qualified as Parser
import AdventOfCode.Prelude hiding (Position (..))
import Data.List (tails)
import Data.Vector.Storable qualified as Vector
import GHC.TypeNats (KnownNat)
import Numeric.LinearAlgebra.Static
  ( L,
    R,
    col,
    extract,
    norm_2,
    row,
    uncol,
    vec2,
    vec3,
    (#),
    (<\>),
    (===),
  )

data Vec = Vec {x, y, z :: !Double}
  deriving (Eq, Show)

data Hailstone = Hailstone {position, velocity :: !Vec}
  deriving (Eq, Show)

solution :: Solution
solution =
  Solution
    { parser = parseHailstone `sepEndBy'` Parser.endOfLine,
      solver = solve1 &&& solve2
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
solve2 hailstones = head $ do
  -- use the first four hailstones to solve the system of equations
  -- three would be enough but with four we always get a unique solution
  h1 : h2 : h3 : h4 : _ <- tails hailstones
  let f x = system h1 x # system h2 x # system h3 x # system h4 x
      j x = jacobian h1 x === jacobian h2 x === jacobian h3 x === jacobian h4 x
      x0 = vec3 0 0 0 # vec3 0 0 0
  maybe
    []
    (pure . round . Vector.sum . Vector.take 3 . extract)
    (newton f j x0)

newton ::
  (KnownNat n, KnownNat m) =>
  (R n -> R m) ->
  (R n -> L m n) ->
  R n ->
  Maybe (R n)
newton f j = go (0 :: Int)
  where
    go !n !x0
      | n >= 20 = Nothing
      | norm_2 y < 1e-6 = Just x0
      | otherwise = go (n + 1) (x0 - uncol (j x0 <\> col y))
      where
        y = f x0

system :: Hailstone -> R 6 -> R 2
system (Hailstone (Vec xi yi zi) (Vec vxi vyi vzi)) s =
  vec2
    ((y - yi) * (vxi - vx) - (x - xi) * (vyi - vy))
    ((z - zi) * (vxi - vx) - (x - xi) * (vzi - vz))
  where
    [x, y, z, vx, vy, vz] = Vector.toList $ extract s

jacobian :: Hailstone -> R 6 -> L 2 6
jacobian (Hailstone (Vec xi yi zi) (Vec vxi vyi vzi)) s =
  row (vec3 (vy - vyi) (vxi - vx) 0 # vec3 (yi - y) (x - xi) 0)
    === row (vec3 (vz - vzi) 0 (vxi - vx) # vec3 (zi - z) 0 (x - xi))
  where
    [x, y, z, vx, vy, vz] = Vector.toList $ extract s
