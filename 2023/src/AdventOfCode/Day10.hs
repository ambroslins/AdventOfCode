module AdventOfCode.Day10 (solution) where

import AdventOfCode.Grid (Direction (..), Grid, Position (..))
import AdventOfCode.Grid qualified as Grid
import AdventOfCode.Prelude
import Data.Foldable (foldlM)
import Data.HashSet qualified as HashSet
import Data.List (find)

data Tile
  = Ground
  | VerticalPipe
  | HorizontalPipe
  | NorthEastPipe
  | NorthWestPipe
  | SouthEastPipe
  | SouthWestPipe
  | Start
  deriving (Eq, Show)

data State = State
  { position :: !Position,
    direction :: !Direction
  }
  deriving (Eq, Show)

solution :: Solution
solution =
  Solution
    { parser = fmap charToTile <$> Grid.parse,
      part1 = solve1,
      part2 = solve2
    }

solve1 :: Grid Tile -> Int
solve1 grid = length loop `div` 2
  where
    loop = fromMaybe (error "solve1: no loop found") $ findLoop grid

solve2 :: Grid Tile -> Int
solve2 grid =
  maybe (error "solve2: no tiles inside loop") HashSet.size $
    tilesLeft <|> tilesRight
  where
    loop = fromMaybe (error "solve2: no loop gound") $ findLoop grid
    tilesLeft = tilesInsideLoop Grid.turnLeft grid loop
    tilesRight = tilesInsideLoop Grid.turnRight grid loop

tilesInsideLoop ::
  (Direction -> Direction) ->
  Grid Tile ->
  [State] ->
  Maybe (HashSet Position)
tilesInsideLoop turn grid loop = go HashSet.empty loop
  where
    onLoop = HashSet.fromList $ map position loop
    go tiles = \case
      [] -> Just tiles
      State {position, direction} : rest -> do
        let nextPos = Grid.move direction position
            dir = turn direction
        ts <- foldlM flood tiles [Grid.move dir position, Grid.move dir nextPos]
        go ts rest
    flood :: HashSet Position -> Position -> Maybe (HashSet Position)
    flood tiles pos
      | pos `HashSet.member` tiles || pos `HashSet.member` onLoop = Just tiles
      | pos `Grid.inside` grid =
          foldlM flood (HashSet.insert pos tiles) $
            [Grid.move dir pos | dir <- [North, East, South, West]]
      | otherwise = Nothing

findLoop :: Grid Tile -> Maybe [State]
findLoop grid = do
  start <- Grid.findPosition (== Start) grid
  let paths =
        [ walk grid state
          | direction <- [North, East, South, West],
            let state = (State {position = start, direction})
        ]
  let isLoop = \case
        _ : _ : _ -> True
        _ -> False
  find isLoop paths

walk :: Grid Tile -> State -> [State]
walk grid state = case step grid state of
  Nothing -> [state]
  Just state' -> state : walk grid state'

step :: Grid Tile -> State -> Maybe State
step grid State {direction, position} = do
  tile <- Grid.index grid pos
  dir <- turnPipe direction tile
  pure State {position = pos, direction = dir}
  where
    pos = Grid.move direction position

turnPipe :: Direction -> Tile -> Maybe Direction
turnPipe dir tile = case (tile, dir) of
  (VerticalPipe, North) -> Just North
  (VerticalPipe, South) -> Just South
  (HorizontalPipe, East) -> Just East
  (HorizontalPipe, West) -> Just West
  (NorthEastPipe, South) -> Just East
  (NorthEastPipe, West) -> Just North
  (NorthWestPipe, South) -> Just West
  (NorthWestPipe, East) -> Just North
  (SouthEastPipe, North) -> Just East
  (SouthEastPipe, West) -> Just South
  (SouthWestPipe, North) -> Just West
  (SouthWestPipe, East) -> Just South
  _ -> Nothing

charToTile :: Char -> Tile
charToTile = \case
  '|' -> VerticalPipe
  '-' -> HorizontalPipe
  'L' -> NorthEastPipe
  'J' -> NorthWestPipe
  '7' -> SouthWestPipe
  'F' -> SouthEastPipe
  'S' -> Start
  _ -> Ground
