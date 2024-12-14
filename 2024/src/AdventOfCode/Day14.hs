module AdventOfCode.Day14 (solution) where

import AdventOfCode.Parser qualified as Parser
import AdventOfCode.Prelude
import AdventOfCode.Vec2 qualified as Vec2
import Control.Monad.ST.Strict (runST)
import Data.Monoid (Sum (..))
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as MVU

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
  mod <$> (position + Vec2.scale seconds velocity) <*> Vec2 width height

solve2 :: [Robot] -> Int
solve2 robots = runST $ do
  xs <- VU.unsafeThaw pxs
  ys <- VU.unsafeThaw pys
  let ps = MVU.zip xs ys
  occupied <- MVU.replicate (width * height) (-1 :: Int)
  let stepX = MVU.iforM_ xs $
        \i x -> MVU.write xs i $ case x + VU.unsafeIndex vxs i of
          x'
            | x' < 0 -> x' + width
            | x' >= width -> x' - width
            | otherwise -> x'
  let stepY = MVU.iforM_ ys $
        \i y -> MVU.write ys i $ case y + VU.unsafeIndex vys i of
          y'
            | y' < 0 -> y' + height
            | y' >= height -> y' - height
            | otherwise -> y'
  let unique !i =
        let go !j
              | j >= MVU.length ps = pure True
              | otherwise = do
                  (x, y) <- MVU.unsafeRead ps j
                  let !hash = x * height + y
                  o <- MVU.exchange occupied hash i
                  if o == i then pure False else go (j + 1)
         in go 0

  let step !i = do
        u <- unique i
        if u then pure i else stepX >> stepY >> step (i + 1)

  step 0
  where
    (pxs, pys, vxs, vys) =
      VU.unzip4 $
        VU.fromList $
          map
            ( \Robot {position = Vec2 px py, velocity = Vec2 vx vy} ->
                (px, py, vx, vy)
            )
            robots

width, height :: Int
width = 101
height = 103
