module AdventOfCode.Day02 (solution) where

import AdventOfCode.Parser qualified as Parser
import AdventOfCode.Prelude
import Data.Char (isAsciiLower)
import Data.Semigroup (Max (..))

data Bag a = Bag
  { red :: !a,
    green :: !a,
    blue :: !a
  }
  deriving (Show, Generic, Functor, Foldable)

instance (Semigroup a) => Semigroup (Bag a) where
  Bag r1 g1 b1 <> Bag r2 g2 b2 = Bag (r1 <> r2) (g1 <> g2) (b1 <> b2)

instance (NFData a) => NFData (Bag a)

type Game = (Int, NonEmpty (Bag Int))

solution :: Solution
solution =
  Solution
    { parser = parseGame `sepEndBy` Parser.endOfLine,
      part1 = solve1,
      part2 = solve2
    }

parseGame :: Parser Game
parseGame = do
  Parser.symbol "Game"
  gameId <- Parser.decimal
  Parser.symbol ":"
  reveales <- reveal `sepBy1` Parser.symbol ";"
  pure (gameId, reveales)
  where
    reveal = toBag <$> cube `sepBy1` Parser.symbol ","
    cube = do
      n <- Parser.lexeme Parser.decimal
      color <- Parser.takeWhile1 isAsciiLower
      pure $ case color of
        "red" -> \bag -> bag {red = n}
        "green" -> \bag -> bag {green = n}
        "blue" -> \bag -> bag {blue = n}
        _ -> error $ "parseGame: invalid color " <> show color
    toBag cs = foldl1' (.) cs (Bag 0 0 0)

solve1 :: [Game] -> Int
solve1 = sum . map (\(i, bags) -> if all isPossible bags then i else 0)

isPossible :: Bag Int -> Bool
isPossible Bag {red, green, blue} = red <= 12 && green <= 13 && blue <= 14

solve2 :: [Game] -> Int
solve2 = sum . map (product . minCubes . snd)

minCubes :: NonEmpty (Bag Int) -> Bag Int
minCubes = fmap getMax . foldMap1' (fmap Max)
