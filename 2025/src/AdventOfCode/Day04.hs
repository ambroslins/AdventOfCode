{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Parenthesize unary negation" #-}

module AdventOfCode.Day04 (solution) where

import AdventOfCode.Parser qualified as Parser
import AdventOfCode.Prelude
import Control.Monad (unless, when)
import Data.ByteString qualified as BS
import Data.Int (Int8)
import Data.Massiv.Array qualified as M
import Data.Massiv.Array.Unsafe qualified as M
import Data.Massiv.Core (Ix2 (..))
import Data.Monoid (Sum (..))

solution :: Solution
solution =
  Solution
    { parser = parseArray,
      solver = solve
    }

parseArray :: (M.Load r Ix2 Int8) => Parser (M.Array r Ix2 Int8)
parseArray = do
  bs <- Parser.takeByteString
  ncols <- maybe (fail "no end of line") pure $ BS.elemIndex (c2w '\n') bs
  let (nrows, rest) = BS.length bs `divMod` (ncols + 1)
      roll = c2w '@'
  unless (rest == 0) $ fail "not rectangular"
  pure $
    M.makeArray M.Seq (M.Sz (nrows :. ncols)) $
      \(row :. col) -> if BS.index bs (row * (ncols + 1) + col) == roll then 1 else 0

solve :: M.Array M.U Ix2 Int8 -> (Int, Int)
solve grid = (solve1 neighbours, solve2 neighbours)
  where
    neighbours = M.compute @M.U $ M.mapStencil (M.Fill 0) stencil grid
    stencil = M.makeUnsafeStencil (M.Sz (3 :. 3)) (1 :. 1) $ \_ get ->
      get (-1 :. -1)
        + get (-1 :. 0)
        + get (-1 :. 1)
        + get (0 :. -1)
        + (1 - get (0 :. 0)) * 100 -- we use values >= 100 to indicate empty fields
        + get (0 :. 1)
        + get (1 :. -1)
        + get (1 :. 0)
        + get (1 :. 1)

solve1 :: (M.Index ix, M.Source r Int8) => M.Array r ix Int8 -> Int
solve1 = getSum . M.foldMono (\i -> if i < 4 then Sum 1 else Sum 0)

solve2 :: (M.Manifest r Int8) => M.Array r Ix2 Int8 -> Int
solve2 grid = solve1 $ M.withMArrayST_ grid $ \mg ->
  let go !ix =
        M.read mg ix >>= \case
          Nothing -> pure ()
          Just n
            | n < 0 || n >= 4 -> pure ()
            | otherwise -> do
                M.writeM mg ix (-1)
                for (-1) 1 $ \dr ->
                  for (-1) 1 $ \dc ->
                    unless (dr == 0 && dc == 0) $
                      let !next = ix + (dr :. dc)
                       in M.modify mg (\x -> pure $ x - 1) next >>= \case
                            Nothing -> pure ()
                            Just m
                              | m < 0 || m > 4 -> pure ()
                              | otherwise -> go next
   in M.iforM_ grid $ \ix n -> when (n < 4) $ go ix
