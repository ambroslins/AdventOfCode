module AdventOfCode.Day10 (solution) where

import AdventOfCode.Grid (Grid)
import AdventOfCode.Grid qualified as Grid
import AdventOfCode.Position qualified as Pos
import AdventOfCode.Prelude
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Vector.Unboxed (Vector)
import Data.Vector.Unboxed qualified as Vector

type Tile = Char

data State = State
  { position :: !Position,
    direction :: !Direction
  }
  deriving (Eq, Show)

solution :: Solution
solution =
  Solution
    { parser = Grid.parse,
      solver = solve
    }

solve :: Grid Vector Tile -> (Int, Int)
solve grid =
  ( IntSet.size loopSet `div` 2,
    tilesInsideLoop start grid loopSet
  )
  where
    loop = fromMaybe (error "solve: no loop found") $ findLoop grid
    start = maybe (error "solve: empty loop") direction $ listToMaybe loop
    loopSet =
      IntSet.fromList $
        map (\State {position = p} -> row p * Grid.ncols grid + col p) loop

tilesInsideLoop :: Direction -> Grid Vector Tile -> IntSet -> Int
tilesInsideLoop start grid loop =
  fst $ Vector.ifoldl' f (0, False) (Grid.cells grid)
  where
    crossed :: String
    crossed = case start of
      North -> "|LJS"
      South -> "|F7S"
      _ -> "|F7"
    f :: (Int, Bool) -> Int -> Char -> (Int, Bool)
    f (!acc, !inside) !i !tile
      | i `IntSet.member` loop =
          (acc, if tile `elem` crossed then not inside else inside)
      | inside = (acc + 1, inside)
      | otherwise = (acc, inside)

findLoop :: Grid Vector Tile -> Maybe [State]
findLoop grid =
  case Grid.findPosition (== 'S') grid of
    Nothing -> Nothing
    Just start ->
      listToMaybe $
        [ walk grid state
          | direction <- [North, South, East],
            let position = Pos.move direction start,
            let state = State {position, direction},
            isJust $ step grid state
        ]

walk :: Grid Vector Tile -> State -> [State]
walk grid state = case step grid state of
  Nothing -> [state]
  Just s -> state : walk grid s

step :: Grid Vector Tile -> State -> Maybe State
step grid State {direction, position} = do
  tile <- Grid.index grid position
  dir <- turnPipe direction tile
  pure State {position = Pos.move dir position, direction = dir}

turnPipe :: Direction -> Tile -> Maybe Direction
turnPipe dir tile = case (tile, dir) of
  ('|', North) -> Just North
  ('|', South) -> Just South
  ('-', East) -> Just East
  ('-', West) -> Just West
  ('L', South) -> Just East
  ('L', West) -> Just North
  ('J', South) -> Just West
  ('J', East) -> Just North
  ('F', North) -> Just East
  ('F', West) -> Just South
  ('7', North) -> Just West
  ('7', East) -> Just South
  _ -> Nothing
