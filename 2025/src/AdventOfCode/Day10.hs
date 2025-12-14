module AdventOfCode.Day10 (solution) where

import AdventOfCode.Parser qualified as Parser
import AdventOfCode.Prelude
import Control.Parallel.Strategies (parMap, rseq)
import Data.Bits (setBit, shiftL, testBit, xor, (.&.))
import Data.ByteString qualified as BS
import Data.Foldable (toList)
import Data.Int (Int16)
import Data.Vector.Unboxed qualified as VU

solution :: Solution
solution =
  Solution
    { parser = parseMachine `sepEndBy'` Parser.endOfLine,
      solver = solve
    }

data Machine = Machine
  { lights :: !Int,
    buttons :: VU.Vector Int,
    joltages :: VU.Vector Int16
  }
  deriving (Show)

parseMachine :: Parser Machine
parseMachine = do
  lights <-
    between (Parser.char '[') (Parser.char ']') $
      BS.foldr' (\c acc -> acc `shiftL` 1 + if c == c2w '#' then 1 else 0) 0
        <$> Parser.takeWhile1 (/= ']')
  Parser.whitespace
  buttons <- VU.fromList <$> parseButton `sepEndBy'` Parser.whitespace
  joltages <-
    between (Parser.char '{') (Parser.char '}') $
      VU.fromList . toList <$> Parser.decimal `sepBy1'` Parser.char ','
  pure Machine {lights, buttons, joltages}
  where
    parseButton =
      between (Parser.char '(') (Parser.char ')') $
        foldl' setBit 0
          <$> Parser.decimal `sepBy1'` Parser.char ','

solve :: [Machine] -> (Int, Int)
solve machines = (sum $ map minPresses machines, sum $ parMap rseq part2 machines)

minPresses :: Machine -> Int
minPresses Machine {lights, buttons} = go 0 lights 0
  where
    size = VU.length buttons
    go !n !acc !i
      | acc == 0 = n
      | i >= size = maxBound
      | otherwise =
          go n acc (i + 1) `min` go (n + 1) (acc `xor` VU.unsafeIndex buttons i) (i + 1)

part2 :: Machine -> Int
part2 Machine {buttons, joltages} = go joltages
  where
    size = VU.length joltages
    expanded = map (expandButton size) $ VU.toList buttons
    notFound = 1_000_000_000_000
    go !js
      | VU.all (== 0) js = 0
      | VU.any (< 0) js = notFound
      | otherwise = case parityMatches js expanded of
          [] -> notFound
          matches ->
            minimum $
              [ m + 2 * go (VU.map (`div` 2) $ VU.zipWith (-) js v)
              | (m, v) <- matches
              ]

expandButton :: Int -> Int -> VU.Vector Int16
expandButton n b = VU.generate n (\i -> if b `testBit` i then 1 else 0)

parityMatches :: VU.Vector Int16 -> [VU.Vector Int16] -> [(Int, VU.Vector Int16)]
parityMatches parity =
  filter (sameParity . snd)
    . ((0, VU.replicate (VU.length parity) 0) :)
    . go
  where
    sameParity = VU.all (\v -> v >= 0 && (v .&. 1) == 0) . VU.zipWith (-) parity
    go :: [VU.Vector Int16] -> [(Int, VU.Vector Int16)]
    go [] = []
    go (x : xs) = (1, x) : foldr f [] (go xs)
      where
        f :: (Int, VU.Vector Int16) -> [(Int, VU.Vector Int16)] -> [(Int, VU.Vector Int16)]
        f (n, y) ys = (n, y) : (n + 1, VU.zipWith (+) x y) : ys
