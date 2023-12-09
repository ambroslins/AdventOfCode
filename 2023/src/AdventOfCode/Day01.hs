module AdventOfCode.Day01 (solution) where

import AdventOfCode.Parser qualified as Parser
import AdventOfCode.Prelude
import Data.ByteString.Char8 qualified as BS
import Data.Char (isDigit, ord)
import Data.Monoid (First (..), Last (..))

solution :: Solution
solution =
  Solution
    { parser = Parser.line `sepEndBy` Parser.endOfLine,
      part1 = solve findDigits,
      part2 = solve findSpelledDigits
    }

solve :: (ByteString -> (First Int, Last Int)) -> [ByteString] -> Int
solve digits = sum . map (combine . digits)
  where
    err = error "solve: no digits found"
    combine (First x, Last y) = fromMaybe err x * 10 + fromMaybe err y

findDigits :: ByteString -> (First Int, Last Int)
findDigits = BS.foldl' go mempty
  where
    go acc c
      | isDigit c =
          let digit = Just $ ord c - ord '0'
           in acc <> (First digit, Last digit)
      | otherwise = acc

findSpelledDigits :: ByteString -> (First Int, Last Int)
findSpelledDigits = foldMap1' go . BS.tailsNE
  where
    go bs = (First digit, Last digit)
      where
        c = BS.head bs
        digit
          | BS.null bs = Nothing
          | isDigit c = Just $! ord c - ord '0'
          | "one" `BS.isPrefixOf` bs = Just 1
          | "two" `BS.isPrefixOf` bs = Just 2
          | "three" `BS.isPrefixOf` bs = Just 3
          | "four" `BS.isPrefixOf` bs = Just 4
          | "five" `BS.isPrefixOf` bs = Just 5
          | "six" `BS.isPrefixOf` bs = Just 6
          | "seven" `BS.isPrefixOf` bs = Just 7
          | "eight" `BS.isPrefixOf` bs = Just 8
          | "nine" `BS.isPrefixOf` bs = Just 9
          | otherwise = Nothing
