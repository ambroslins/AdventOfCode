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
  (<>) = zipBag (<>)

instance (Monoid a) => Monoid (Bag a) where
  mempty = Bag mempty mempty mempty
  mappend = (<>)

instance (NFData a) => NFData (Bag a)

zipBag :: (a -> b -> c) -> Bag a -> Bag b -> Bag c
zipBag f (Bag r1 g1 b1) (Bag r2 g2 b2) = Bag (f r1 r2) (f g1 g2) (f b1 b2)

type Game = (Int, [Bag Int])

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
  reveales <- (toBag <$> Parser.list cube) `sepBy` Parser.symbol ";"

  pure (gameId, reveales)
  where
    cube = do
      n <- Parser.lexeme Parser.decimal
      c <- Parser.takeWhile1 isAsciiLower
      pure (c, n)
    toBag cs =
      fromMaybe 0
        <$> Bag
          { red = lookup "red" cs,
            green = lookup "green" cs,
            blue = lookup "blue" cs
          }

bag1 :: Bag Int
bag1 = Bag {red = 12, green = 13, blue = 14}

solve1 :: [Game] -> Int
solve1 = sum . map (\(gameId, bags) -> if all possible bags then gameId else 0)

possible :: Bag Int -> Bool
possible b = and (zipBag (<=) b bag1)

solve2 :: [Game] -> Int
solve2 = sum . map (product . minCubes . snd)

minCubes :: [Bag Int] -> Bag Int
minCubes = fmap getMax . foldMap' (fmap Max)
