module AdventOfCode.Day03 (solution) where

import AdventOfCode.Parser qualified as Parser
import AdventOfCode.Prelude
import Data.ByteString qualified as BS
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet

solution :: Solution
solution =
  Solution
    { parser = parseParts,
      part1 = solve1,
      part2 = solve2
    }

data Part = Number Int | Symbol Char
  deriving (Show, Generic)

instance NFData Part

type Pos = (Int, Int)

parseRow :: Int -> Parser [(Int, Int, Part)]
parseRow !offset = do
  (n, _) <- withOffset (Parser.takeWhile (== '.'))
  (l, part) <- withOffset (number <|> symbol)
  let p = (offset + n, l, part)
  ps <- parseRow (offset + n + l) <|> pure []
  Parser.skipWhile (== '.')
  pure (p : ps)
  where
    number = Number <$> Parser.decimal
    symbol = Symbol <$> Parser.satisfy (Parser.notInClass ".\n\r")
    withOffset p = first BS.length <$> Parser.match p

parseParts :: Parser [(Pos, Int, Part)]
parseParts = enumerate <$> parseRow 0 `sepEndBy` Parser.endOfLine
  where
    enumerate parts = do
      (y, row) <- zip [0 ..] parts
      (x, l, part) <- row
      pure ((x, y), l, part)

adjacents :: (Int, Int) -> Int -> [(Int, Int)]
adjacents (x, y) l = do
  dx <- [-1 .. l]
  dy <- [-1 .. 1]
  pure (x + dx, y + dy)

solve1 :: [(Pos, Int, Part)] -> Int
solve1 parts = sum $ do
  ((x, y), l, Number n) <- parts
  guard $ any (`HashSet.member` symbols) $ adjacents (x, y) l
  pure n
  where
    symbols = HashSet.fromList [pos | (pos, _, Symbol _) <- parts]

solve2 :: [(Pos, Int, Part)] -> Int
solve2 parts = sum $ do
  ((x, y), _, Symbol '*') <- parts
  let adj = do
        dx <- [-3 .. 1]
        dy <- [-1 .. 1]
        case HashMap.lookup (x + dx, y + dy) numbers of
          Just (l, n) | l >= -dx -> pure n
          _ -> []
  case adj of
    [n1, n2] -> pure (n1 * n2)
    _ -> []
  where
    numbers = HashMap.fromList $ [(pos, (l, n)) | (pos, l, Number n) <- parts]
