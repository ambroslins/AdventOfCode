module AdventOfCode.Day02 (solution) where

import AdventOfCode.Parser qualified as Parser
import AdventOfCode.Prelude
import Data.Char (isAsciiLower)

data Game = Game {gameId :: !Int, red :: !Int, green :: !Int, blue :: !Int}
  deriving (Show)

solution :: Solution
solution =
  Solution
    { parser = parseGame `sepEndBy'` Parser.endOfLine,
      part1 = solve1,
      part2 = solve2
    }

parseGame :: Parser Game
parseGame = do
  Parser.symbol "Game"
  gameId <- Parser.decimal
  Parser.symbol ":"
  foldl' (flip ($)) (Game gameId 0 0 0) <$> cube `sepBy1` sep
  where
    sep = Parser.symbol "," <|> Parser.symbol ";"
    cube :: Parser (Game -> Game)
    cube = do
      n <- Parser.lexeme Parser.decimal
      color <- Parser.takeWhile1 isAsciiLower
      pure $ case color of
        "red" -> \b -> b {red = max n (red b)}
        "green" -> \b -> b {green = max n (green b)}
        "blue" -> \b -> b {blue = max n (blue b)}
        _ -> error $ "parseGame: invalid color " <> show color

solve1 :: [Game] -> Int
solve1 = sum . map gameId . filter isPossible

isPossible :: Game -> Bool
isPossible Game {red, green, blue} = red <= 12 && green <= 13 && blue <= 14

solve2 :: [Game] -> Int
solve2 = sum . map power

power :: Game -> Int
power Game {red, green, blue} = red * green * blue
