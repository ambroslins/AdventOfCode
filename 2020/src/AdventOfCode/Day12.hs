module AdventOfCode.Day12 where

import AdventOfCode.Prelude

data Instruction = Instruction Action Int
  deriving (Eq, Show)

data Direction = North | South | East | West
  deriving (Eq, Show)

data Action = Move Direction | L | R | Forward
  deriving (Eq, Show)

data Ship = Ship Direction Int Int
  deriving (Eq, Show)

parseDirection :: Parser Direction
parseDirection =
  choice
    [ North <$ char 'N',
      South <$ char 'S',
      East <$ char 'E',
      West <$ char 'W'
    ]

parseAction :: Parser Action
parseAction =
  choice
    [ Move <$> parseDirection,
      L <$ char 'L',
      R <$ char 'R',
      Forward <$ char 'F'
    ]

parseInstruction :: Parser Instruction
parseInstruction = Instruction <$> parseAction <*> decimal

parser :: Parser [Instruction]
parser = parseInstruction `sepEndBy` eol

rotateR :: Direction -> Direction
rotateR = \case
  North -> East
  East -> South
  South -> West
  West -> North

rotateL :: Direction -> Direction
rotateL = \case
  North -> West
  East -> North
  South -> East
  West -> South

exec :: Instruction -> Ship -> Ship
exec (Instruction action value) (Ship dir x y) = case action of
  Move North -> Ship dir x (y + value)
  Move South -> Ship dir x (y - value)
  Move East -> Ship dir (x + value) y
  Move West -> Ship dir (x - value) y
  R -> Ship (iterate rotateR dir !! (value `div` 90)) x y
  L -> Ship (iterate rotateL dir !! (value `div` 90)) x y
  Forward -> exec (Instruction (Move dir) value) (Ship dir x y)

solution :: Solution
solution = Solution parser solve1 solve2

solve1 :: [Instruction] -> Int
solve1 xs = let Ship _ x y = foldl (flip exec) (Ship East 0 0) xs in abs x + abs y

data Waypoint = Waypoint Int Int
  deriving (Eq, Show)

data Ship2 = Ship2 Int Int Waypoint
  deriving (Eq, Show)

rotateWaypointR :: Waypoint -> Waypoint
rotateWaypointR (Waypoint x y) = Waypoint y (- x)

rotateWaypointL :: Waypoint -> Waypoint
rotateWaypointL (Waypoint x y) = Waypoint (- y) x

exec2 :: Instruction -> Ship2 -> Ship2
exec2 (Instruction action value) (Ship2 x y w@(Waypoint dx dy)) = case action of
  Move North -> Ship2 x y $ Waypoint dx (dy + value)
  Move South -> Ship2 x y $ Waypoint dx (dy - value)
  Move East -> Ship2 x y $ Waypoint (dx + value) dy
  Move West -> Ship2 x y $ Waypoint (dx - value) dy
  R -> Ship2 x y $ iterate rotateWaypointR w !! (value `div` 90)
  L -> Ship2 x y $ iterate rotateWaypointL w !! (value `div` 90)
  Forward -> Ship2 (x + dx * value) (y + dy * value) w

solve2 :: [Instruction] -> Int
solve2 xs = let Ship2 x y _ = foldl (flip exec2) (Ship2 0 0 (Waypoint 10 1)) xs in abs x + abs y
