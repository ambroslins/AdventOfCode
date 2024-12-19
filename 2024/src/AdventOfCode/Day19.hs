module AdventOfCode.Day19 (solution) where

import AdventOfCode.Parser qualified as Parser
import AdventOfCode.Prelude
import Control.Monad (foldM)
import Control.Parallel.Strategies (parMap, rseq)
import Data.ByteString qualified as BS
import Data.Char (isAsciiLower)
import Data.HashSet qualified as HashSet

type Towel = ByteString

type Design = ByteString

solution :: Solution
solution =
  Solution
    { parser = do
        towels <- parseTowel `sepBy'` Parser.symbol ", "
        Parser.endOfLine
        Parser.endOfLine
        designs <- parseDesing `sepEndBy'` Parser.endOfLine
        pure (towels, designs),
      solver = uncurry solve1 &&& length . snd
    }

solve1 :: [Towel] -> [Design] -> Int
solve1 towels = count id . parMap rseq (possible towels)

parseTowel :: Parser Towel
parseTowel = Parser.takeWhile1 isAsciiLower

parseDesing :: Parser Design
parseDesing = Parser.takeWhile1 isAsciiLower

possible :: [Towel] -> Design -> Bool
possible towels = isNothing . go HashSet.empty
  where
    go !seen design
      | BS.null design = Nothing
      | design `HashSet.member` seen = Just seen
      | otherwise =
          foldM go (HashSet.insert design seen) $
            mapMaybe (`BS.stripPrefix` design) towels
