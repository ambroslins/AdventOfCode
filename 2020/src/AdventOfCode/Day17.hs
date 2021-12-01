module AdventOfCode.Day17 where

import AdventOfCode.Prelude
import Data.List (unzip4)
import qualified Data.Set as Set

type Cube = (Int, Int, Int)

-- | Set of active Cubes
type EnergySource = Set Cube

parseInput :: String -> EnergySource
parseInput =
  Set.fromList
    . catMaybes
    . concat
    . imap (\y -> imap (\x c -> if c == '#' then Just (x, y, 0) else Nothing))
    . lines

imap :: (Int -> a -> b) -> [a] -> [b]
imap f = zipWith f [0 ..]

solution :: Solution
solution = simpleSolution parseInput solve1 solve2

neighbors :: Cube -> [Cube]
neighbors (x, y, z) =
  [ (x + dx, y + dy, z + dz)
    | dx <- [-1 .. 1],
      dy <- [-1 .. 1],
      dz <- [-1 .. 1],
      dx /= 0 || dy /= 0 || dz /= 0
  ]

wrap :: EnergySource -> [Cube]
wrap es = [(x, y, z) | x <- extend xs, y <- extend ys, z <- extend zs]
  where
    (xs, ys, zs) = unzip3 $ Set.toList es
    extend x = [(minimum x - 1) .. (maximum x + 1)]

execCycle :: EnergySource -> EnergySource
execCycle es = Set.fromList $ filter f $ wrap es
  where
    f c =
      let activeNeighbors = length $ filter (`Set.member` es) $ neighbors c
       in if c `Set.member` es
            then activeNeighbors `elem` [2, 3]
            else activeNeighbors == 3

solve1 :: EnergySource -> Int
solve1 = Set.size . (!! 6) . iterate execCycle

type Cube4 = (Int, Int, Int, Int)

neighbors4 :: Cube4 -> [Cube4]
neighbors4 (x, y, z, w) =
  [ (x + dx, y + dy, z + dz, w + dw)
    | dx <- [-1 .. 1],
      dy <- [-1 .. 1],
      dz <- [-1 .. 1],
      dw <- [-1 .. 1],
      dx /= 0 || dy /= 0 || dz /= 0 || dw /= 0
  ]

wrap4 :: Set Cube4 -> [Cube4]
wrap4 es = [(x, y, z, w) | x <- extend xs, y <- extend ys, z <- extend zs, w <- extend ws]
  where
    (xs, ys, zs, ws) = unzip4 $ Set.toList es
    extend x = [(minimum x - 1) .. (maximum x + 1)]

execCycle4 :: Set Cube4 -> Set Cube4
execCycle4 es = Set.fromList $ filter f $ wrap4 es
  where
    f c =
      let activeNeighbors = length $ filter (`Set.member` es) $ neighbors4 c
       in if c `Set.member` es
            then activeNeighbors `elem` [2, 3]
            else activeNeighbors == 3

solve2 :: EnergySource -> Int
solve2 = Set.size . (!! 6) . iterate execCycle4 . Set.mapMonotonic (\(x, y, z) -> (x, y, z, 0))