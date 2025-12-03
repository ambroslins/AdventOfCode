{-# LANGUAGE ViewPatterns #-}

module AdventOfCode.Day03 (solution) where

import AdventOfCode.Parser qualified as Parser
import AdventOfCode.Prelude
import Data.ByteString qualified as BS
import Data.Char (ord)
import Data.Coerce (coerce)
import Data.Monoid (Sum (..))
import Data.Word (Word8)

solution :: Solution
solution =
  Solution
    { parser = parseLine `sepEndBy'` Parser.endOfLine,
      solver = solve
    }

parseLine :: Parser ByteString
parseLine = Parser.line

solve :: [ByteString] -> (Int, Int)
solve = coerce . foldMap' go
  where
    go bank = (Sum $ joltage 2 bank, Sum $ joltage 12 bank)

joltage :: Int -> ByteString -> Int
joltage = go 0
  where
    go !acc !digits !bank
      | digits <= 0 = acc
      | otherwise =
          let (!i, !m) = maximumIndex $ BS.dropEnd (digits - 1) bank
           in go (acc * 10 + fromIntegral m - ord '0') (digits - 1) (BS.drop (i + 1) bank)

maximumIndex :: ByteString -> (Int, Word8)
maximumIndex (BS.elemIndex 57 -> Just i) = (i, 57) -- check for '9' and '8' first as they are more likely
maximumIndex (BS.elemIndex 56 -> Just i) = (i, 56)
maximumIndex bs =
  -- general case
  let !m = BS.maximum bs
   in (fromMaybe (error "maximumIndex: impossible") $ BS.elemIndex m bs, m)
