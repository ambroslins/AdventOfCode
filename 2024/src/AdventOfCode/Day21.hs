module AdventOfCode.Day21 (solution) where

import AdventOfCode.Parser qualified as Parser
import AdventOfCode.Position qualified as Pos
import AdventOfCode.Prelude
import Control.Exception (assert)
import Control.Parallel.Strategies (parMap, rseq)
import Data.ByteString.Char8 qualified as BS
import Data.Coerce (coerce)
import Data.List qualified as List
import Data.Monoid (Sum (..))

type Key = Position

solution :: Solution
solution =
  Solution
    { parser = Parser.match parseCode `sepEndBy` Parser.endOfLine,
      solver = solve
    }

parseCode :: Parser Int
parseCode = Parser.decimal <* Parser.line

solve :: [(ByteString, Int)] -> (Int, Int)
solve = coerce . foldMap' id . parMap rseq (uncurry solveCode)

solveCode :: ByteString -> Int -> (Sum Int, Sum Int)
solveCode keys code = (Sum $ code * presses !! 3, Sum $ code * presses !! 26)
  where
    presses = map length $ iterate pressAll $ map numericKey $ BS.unpack keys

numericKey :: Char -> Key
numericKey = \case
  '0' -> Position {row = 0, col = 1}
  '1' -> Position {row = -1, col = 0}
  '2' -> Position {row = -1, col = 1}
  '3' -> Position {row = -1, col = 2}
  '4' -> Position {row = -2, col = 0}
  '5' -> Position {row = -2, col = 1}
  '6' -> Position {row = -2, col = 2}
  '7' -> Position {row = -3, col = 0}
  '8' -> Position {row = -3, col = 1}
  '9' -> Position {row = -3, col = 2}
  'A' -> aKey
  c -> error $ "unexpected numeric key: " <> show c

upKey, aKey, leftKey, downKey, rightKey :: Key
upKey = Position {row = 0, col = 1}
aKey = Position {row = 0, col = 2}
leftKey = Position {row = 1, col = 0}
downKey = Position {row = 1, col = 1}
rightKey = Position {row = 1, col = 2}

pressAll :: [Key] -> [Key]
pressAll keys = concat $ zipWith press (aKey : keys) keys

press :: Key -> Key -> [Key]
press !start !target
  | row start == 0 && col target == 0 =
      concat
        [ replicate (-dr) upKey,
          replicate dr downKey,
          replicate (-dc) leftKey,
          [aKey]
        ]
  | col start == 0 && row target == 0 =
      concat
        [ replicate dc rightKey,
          replicate dr downKey,
          replicate (-dr) upKey,
          [aKey]
        ]
  | otherwise =
      concat
        [ replicate (-dc) leftKey,
          replicate dr downKey,
          replicate dc rightKey,
          replicate (-dr) upKey,
          [aKey]
        ]
  where
    !dr = row target - row start
    !dc = col target - col start

invertNum :: Key -> Char
invertNum k =
  fromJust $ List.lookup k [(numericKey c, c) | c <- 'A' : ['0' .. '9']]

simulate :: (Show a) => (Key -> a) -> [Key] -> [a]
simulate result = go aKey
  where
    go pos = \case
      [] -> []
      (k : ks) ->
        assert (pos /= dead) $
          fromJust $
            List.lookup
              k
              [ (upKey, go (Pos.move North pos) ks),
                (aKey, result pos : go pos ks),
                (leftKey, go (Pos.move West pos) ks),
                (downKey, go (Pos.move South pos) ks),
                (rightKey, go (Pos.move East pos) ks)
              ]

undo :: [Key] -> String
undo = simulate invertNum . simulate id . simulate id

dead :: Position
dead = Position {row = 0, col = 0}
