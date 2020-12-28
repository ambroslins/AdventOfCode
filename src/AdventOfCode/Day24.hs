module AdventOfCode.Day24 where

import AdventOfCode.Prelude
import Data.Bool (bool)
import Data.Foldable
import qualified Data.Set as Set

type Tile = (Int, Int, Int)

data Direction = E | SE | SW | W | NW | NE
  deriving (Eq, Show)

parseDirection :: Parser Direction
parseDirection =
  choice
    [ E <$ string "e",
      SE <$ string "se",
      SW <$ string "sw",
      W <$ string "w",
      NW <$ string "nw",
      NE <$ string "ne"
    ]

move :: Tile -> Direction -> Tile
move (x, y, z) = \case
  E -> (x + 1, y - 1, z)
  SE -> (x + 1, y, z - 1)
  SW -> (x, y + 1, z - 1)
  W -> (x - 1, y + 1, z)
  NW -> (x -1, y, z + 1)
  NE -> (x, y -1, z + 1)

solution :: Solution
solution = Solution (some parseDirection `sepEndBy` eol) solve1 solve2

solve1 :: [[Direction]] -> Int
solve1 = Set.size . foldr (alter . foldl move (0, 0, 0)) Set.empty

alter :: Ord a => a -> Set a -> Set a
alter x xs = bool (Set.insert x) (Set.delete x) (x `Set.member` xs) xs

solve2 :: [[Direction]] -> Int
solve2 input = Set.size $ (!! 100) $ iterate evalDay tiles
  where
    tiles = foldr (alter . foldl' move (0, 0, 0)) Set.empty input

evalDay :: Set Tile -> Set Tile
evalDay tiles =
  Set.filter
    ( \t ->
        let ts = Set.filter (`Set.member` tiles) $ adjacent t
         in if t `Set.member` tiles
              then Set.size ts `elem` [1, 2]
              else Set.size ts == 2
    )
    allTiles
  where
    adjacent x = Set.fromList $ map (move x) [E, SE, SW, W, NW, NE]
    allTiles = Set.unions $ map (\x -> Set.insert x $ adjacent x) $ Set.toList tiles
