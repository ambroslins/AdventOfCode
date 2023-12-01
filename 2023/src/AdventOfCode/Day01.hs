module AdventOfCode.Day01 (solution) where

import AdventOfCode.Parser qualified as Parser
import AdventOfCode.Prelude
import Control.Monad (guard)
import Data.ByteString.Char8 qualified as BS
import Data.Char (isAlphaNum, isDigit)
import Data.List (find)
import Data.Maybe (mapMaybe)

solution :: Solution
solution =
  Solution
    { parser = Parser.takeWhile1 isAlphaNum `sepEndBy1` Parser.endOfLine,
      part1 = solve parseDigit,
      part2 = solve (\xs -> parseDigit xs <|> parseSpelledDigit xs)
    }

solve :: (ByteString -> Maybe Int) -> [ByteString] -> Int
solve f =
  sum
    . map
      ( \xs ->
          let digits = mapMaybe f (BS.tails xs)
           in head digits * 10 + last digits
      )

parseDigit :: ByteString -> Maybe Int
parseDigit line = do
  (x, _) <- BS.uncons line
  guard (isDigit x)
  pure (read [x])

parseSpelledDigit :: ByteString -> Maybe Int
parseSpelledDigit line = do
  (value, _) <-
    find (\(_, spelled) -> spelled `BS.isPrefixOf` line) $
      zip
        [1 ..]
        ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
  pure value
