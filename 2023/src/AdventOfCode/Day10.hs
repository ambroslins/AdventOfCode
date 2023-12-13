module AdventOfCode.Day10 (solution) where

import AdventOfCode.Grid (Grid)
import AdventOfCode.Grid qualified as Grid
import AdventOfCode.Position qualified as Pos
import AdventOfCode.Prelude
import Data.Foldable (foldlM)
import Data.HashSet qualified as HashSet
import Data.List (find)
import Data.Vector.Unboxed (Vector)

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
      part1 = solve1,
      part2 = solve2
    }

solve1 :: Grid Vector Tile -> Int
solve1 grid = length loop `div` 2
  where
    loop = fromMaybe (error "solve1: no loop found") $ findLoop grid

solve2 :: Grid Vector Tile -> Int
solve2 grid =
  maybe (error "solve2: no tiles inside loop") HashSet.size $
    tilesLeft <|> tilesRight
  where
    loop = fromMaybe (error "solve2: no loop gound") $ findLoop grid
    tilesLeft = tilesInsideLoop Pos.turnLeft grid loop
    tilesRight = tilesInsideLoop Pos.turnRight grid loop

tilesInsideLoop ::
  (Direction -> Direction) ->
  Grid Vector Tile ->
  [State] ->
  Maybe (HashSet Position)
tilesInsideLoop turn grid loop = go HashSet.empty loop
  where
    onLoop = HashSet.fromList $ map position loop
    go tiles = \case
      [] -> Just tiles
      State {position, direction} : rest -> do
        let nextPos = Pos.move direction position
            dir = turn direction
        ts <- foldlM flood tiles [Pos.move dir position, Pos.move dir nextPos]
        go ts rest
    flood :: HashSet Position -> Position -> Maybe (HashSet Position)
    flood tiles pos
      | pos `HashSet.member` tiles || pos `HashSet.member` onLoop = Just tiles
      | pos `Grid.inside` grid =
          foldlM flood (HashSet.insert pos tiles) $
            [Pos.move dir pos | dir <- [North, East, South, West]]
      | otherwise = Nothing

findLoop :: Grid Vector Tile -> Maybe [State]
findLoop grid = do
  start <- Grid.findPosition (== 'S') grid
  let paths =
        [ walk grid state
          | direction <- [North, East, South, West],
            let state = (State {position = start, direction})
        ]
  let isLoop = \case
        _ : _ : _ -> True
        _ -> False
  find isLoop paths

walk :: Grid Vector Tile -> State -> [State]
walk grid state = case step grid state of
  Nothing -> [state]
  Just state' -> state : walk grid state'

step :: Grid Vector Tile -> State -> Maybe State
step grid State {direction, position} = do
  tile <- Grid.index grid pos
  dir <- turnPipe direction tile
  pure State {position = pos, direction = dir}
  where
    pos = Pos.move direction position

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
