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

data V12 a = V12 !a !a !a !a !a !a !a !a !a !a !a !a
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance Applicative V12 where
  pure x = V12 x x x x x x x x x x x x
  V12 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12
    <*> V12 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 =
      V12 (f1 x1) (f2 x2) (f3 x3) (f4 x4) (f5 x5) (f6 x6) (f7 x7) (f8 x8) (f9 x9) (f10 x10) (f11 x11) (f12 x12)

fromList :: a -> [a] -> V12 a
fromList zero l = case l ++ repeat zero of
  x1 : x2 : x3 : x4 : x5 : x6 : x7 : x8 : x9 : x10 : x11 : x12 : _ ->
    V12 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12
  _ -> error "impossible: finite list"

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

{- 10,11,11,5,10,5 - 2,1,1,1,2,1
 -  8,10,10,4,8,4
 -  4, 5, 5,2,4,2
 -
 -}

{-
parityPresses :: VU.Vector Int16 -> [VU.Vector Int16] -> [VU.Vector Int16]
parityPresses parity =
  filter (VU.all (\v -> not $ v `testBit` 0) . VU.zipWith xor parity)
    . mapMaybe (fmap (foldl1' $ VU.zipWith (+)) . nonEmpty)
    . List.subsequences
-}

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

{-

makeSystem :: Machine -> Matrix U Double
makeSystem Machine {buttons, joltage} =
  M.resize' (Sz2 n (m + 1)) . M.fromUnboxedVector Seq . V.concat . List.sortBy (flip compare) $
    [ V.fromListN (m + 1) $ map (\b -> if b `testBit` i then 1 else 0) buttons ++ [fromIntegral j]
    | (i, j) <- zip [0 ..] joltage
    ]
  where
    n = length joltage
    m = length buttons

solveSystem :: Matrix U Double -> Matrix U Double
solveSystem system = runST $ do
  sys <- M.thawS system
  for 0 (min (m - 2) (n - 1)) $ \pivot -> do
    p <- M.readM sys (Ix2 pivot pivot)
    when (p /= 0) $ for (pivot + 1) (n - 1) $ \i -> do
      x <- M.readM sys (Ix2 i pivot)
      let r = x / p
      when (x /= 0.0) $ for pivot (m - 1) $ \j ->
        M.modifyM_
          sys
          (\y -> do z <- M.readM sys (Ix2 pivot j); pure (y - r * z))
          (Ix2 i j)

  M.freezeS sys
  where
    Sz2 n m = M.size system
-}
