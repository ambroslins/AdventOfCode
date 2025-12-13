module AdventOfCode.Day12 (solution) where

import AdventOfCode.Parser qualified as Parser
import AdventOfCode.Prelude
import Data.ByteString qualified as BS

solution :: Solution
solution =
  Solution
    { parser = do
        shapes <- parseShape `sepEndBy'` Parser.endOfLine
        regions <- parseRegion `sepEndBy'` Parser.endOfLine
        pure (shapes, regions),
      solver = uncurry solve
    }

data Region = Region {width :: !Int, height :: !Int, quantities :: [Int]}
  deriving (Show)

parseShape :: Parser Int
parseShape = do
  _ <- Parser.digit
  _ <- Parser.char ':'
  Parser.endOfLine
  bs <- Parser.take 12
  pure $ BS.foldl' (\acc c -> acc + if c == c2w '#' then 1 else 0) 0 bs

parseRegion :: Parser Region
parseRegion = do
  width <- Parser.decimal
  _ <- Parser.char 'x'
  height <- Parser.decimal
  _ <- Parser.symbol ":"
  quantities <- Parser.decimal `sepBy'` Parser.whitespace
  pure Region {width, height, quantities}

solve :: [Int] -> [Region] -> (Int, Int)
solve _ regions = (count estimate regions, 0)
  where
    estimate Region {width, height, quantities} =
      width * height >= foldl' (\acc q -> acc + 9 * q) 0 quantities
