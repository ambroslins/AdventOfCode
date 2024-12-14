module AdventOfCode.Day14 (solution) where

import AdventOfCode.Parser qualified as Parser
import AdventOfCode.Prelude
import AdventOfCode.Vec2 qualified as Vec2
import Data.IntSet qualified as IntSet
import Data.Monoid (Sum (..))

data Robot = Robot {position :: !(Vec2 Int), velocity :: !(Vec2 Int)}
  deriving (Show)

solution :: Solution
solution =
  Solution
    { parser = parseRobot `sepEndBy` Parser.endOfLine,
      solver = solve1 &&& solve2
    }

parseRobot :: Parser Robot
parseRobot = do
  Parser.symbol "p="
  position <- parseVec2
  Parser.symbol " v="
  velocity <- parseVec2
  pure $ Robot {position, velocity}

parseVec2 :: Parser (Vec2 Int)
parseVec2 = do
  x <- Parser.int
  Parser.symbol ","
  y <- Parser.int
  pure $ Vec2 x y

solve1 :: [Robot] -> Int
solve1 robots = q1 * q2 * q3 * q4
  where
    (Sum q1, Sum q2, Sum q3, Sum q4) = foldMap' go robots
    go robot = case move 100 robot of
      Vec2 x y
        | x < 50 && y < 51 -> (1, 0, 0, 0)
        | x > 50 && y < 51 -> (0, 1, 0, 0)
        | x < 50 && y > 51 -> (0, 0, 1, 0)
        | x > 50 && y > 51 -> (0, 0, 0, 1)
        | otherwise -> (0, 0, 0, 0)

move :: Int -> Robot -> Vec2 Int
move seconds Robot {position, velocity} =
  mod <$> (position + Vec2.scale seconds velocity) <*> Vec2 101 103

solve2 :: [Robot] -> Int
solve2 robots = go 0 robots
  where
    n = length robots
    step r = r {position = move 1 r}
    go !i rs
      | IntSet.size (IntSet.fromList $ map (hash . position) rs) == n = i
      | otherwise = go (i + 1) (map step rs)
    hash (Vec2 x y) = x * 103 + y
