module AdventOfCode.Day21 (solution) where

import AdventOfCode.Parser qualified as Parser
import AdventOfCode.Prelude
import Control.Monad (zipWithM)
import Control.Monad.ST.Strict (runST)
import Data.ByteString.Char8 qualified as BS
import Data.Vector.Unboxed.Mutable qualified as MVU
import Debug.Trace (traceShowWith)

type Key = Position

solution :: Solution
solution =
  Solution
    { parser = Parser.match parseCode `sepEndBy` Parser.endOfLine,
      solver = solve
    }

parseCode :: Parser Int
parseCode = Parser.decimal <* Parser.line

-- not: 189357384273226
solve :: [(ByteString, Int)] -> (Int, Int)
solve codes =
  ( sum $ map (uncurry $ solveCode 2) codes,
    sum $ map (uncurry $ solveCode 25) codes
  )

solveCode :: Int -> ByteString -> Int -> Int
solveCode robots keys code = traceShowWith (keys,) $ runST $ do
  cache <- MVU.replicate (6 * 6 * robots) (0 :: Int)
  let presses !n !start !target
        | n >= robots = pure 1
        | otherwise = do
            let i = (hash start * 6 + hash target) * robots + n
            c <- MVU.read cache i
            if c > 0
              then pure c
              else do
                let nextKeys = press start target
                c' <- sum <$> zipWithM (presses (n + 1)) (aKey : nextKeys) nextKeys
                MVU.write cache i c'
                pure c'

  let ks = pressAll $ map numericKey $ BS.unpack keys
  n <- sum <$> zipWithM (presses 0) (aKey : ks) ks
  pure $ code * n
  where
    hash p = row p * 3 + col p

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
