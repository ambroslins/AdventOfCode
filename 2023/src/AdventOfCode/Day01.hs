module AdventOfCode.Day01 (solution) where

import AdventOfCode.Parser qualified as Parser
import AdventOfCode.Prelude
import Data.ByteString.Char8 qualified as BS
import Data.Char (isAlphaNum, isDigit, ord)
import Data.Function (fix)
import Data.List (find)

solution :: Solution
solution =
  Solution
    { parser = Parser.takeWhile1 isAlphaNum `sepEndBy1` Parser.endOfLine,
      part1 = solve parseDigit,
      part2 = solve parseSpelledDigit
    }

solve :: (ByteString -> [Int]) -> [ByteString] -> Int
solve f =
  sum . map (\l -> let ds = f l in head ds * 10 + last ds)

parseDigit :: ByteString -> [Int]
parseDigit =
  BS.foldr (\c ds -> if isDigit c then (ord c - ord '0') : ds else ds) []

parseSpelledDigit :: ByteString -> [Int]
parseSpelledDigit =
  fix $ \rec line -> case BS.uncons line of
    Nothing -> []
    Just (c, rest)
      | isDigit c -> (ord c - ord '0') : rec rest
      | otherwise ->
          case find (\(_, d) -> d `BS.isPrefixOf` line) spelled of
            Nothing -> rec rest
            Just (x, d) -> x : rec (BS.drop (BS.length d - 1) line)
  where
    spelled =
      zip
        [1 ..]
        ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
