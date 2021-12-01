module AdventOfCode.Day11 where

import AdventOfCode.Prelude
import qualified Data.Vector as Vector

data Seat = Floor | Empty | Occupied
  deriving (Eq, Show)

isOccupied :: Seat -> Bool
isOccupied = (== Occupied)

type SeatLayout = Vector (Vector Seat)

parseSeat :: Parser Seat
parseSeat =
  choice
    [ Floor <$ char '.',
      Empty <$ char 'L',
      Occupied <$ char '#'
    ]

solution :: Solution
solution = Solution parser solve1 solve2

parser :: Parser SeatLayout
parser = Vector.fromList <$> ((Vector.fromList <$> some parseSeat) `sepEndBy` eol)

countOccupied :: SeatLayout -> Int
countOccupied = length . filter isOccupied . concat . map Vector.toList . Vector.toList

solve1 :: SeatLayout -> Int
solve1 = countOccupied . fix (step rule1)

solve2 :: SeatLayout -> Int
solve2 = countOccupied . fix (step rule2)

fix :: Eq a => (a -> a) -> a -> a
fix f x = let x' = f x in if x == x' then x else fix f x'

type Rule = Int -> Int -> SeatLayout -> Seat -> Seat

step :: Rule -> SeatLayout -> SeatLayout
step rule xs =
  Vector.imap
    ( \y ->
        Vector.imap (\x -> rule x y xs)
    )
    xs

rule1 :: Rule
rule1 x y xs =
  let adj = adjacent x y xs
   in \case
        Empty | null (filter isOccupied adj) -> Occupied
        Occupied | length (filter isOccupied adj) >= 4 -> Empty
        s -> s

adjacent :: Int -> Int -> SeatLayout -> [Seat]
adjacent x y xs =
  catMaybes $
    map
      (\(x', y') -> (xs !? y') >>= (!? x'))
      [(x + dx, y + dy) | (dx, dy) <- directions]

directions :: [(Int, Int)]
directions = [(dx, dy) | dy <- [-1 .. 1], dx <- [-1 .. 1], dx /= 0 || dy /= 0]

rule2 :: Rule
rule2 x y xs =
  let vis = visible x y xs
   in \case
        Empty | null (filter isOccupied vis) -> Occupied
        Occupied | length (filter isOccupied vis) >= 5 -> Empty
        s -> s

visible :: Int -> Int -> SeatLayout -> [Seat]
visible x y seats = map (firstSeen . ray) directions
  where
    firstSeen (Just s : xs) = case s of
      Floor -> firstSeen xs
      _ -> s
    firstSeen _ = Floor
    addTuple (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
    ray dir = map (\(x', y') -> (seats !? y') >>= (!? x')) $ tail $ iterate (addTuple dir) (x, y)