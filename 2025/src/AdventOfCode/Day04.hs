{-# HLINT ignore "Use for_" #-}
{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Parenthesize unary negation" #-}

module AdventOfCode.Day04 (solution) where

import AdventOfCode.Parser qualified as Parser
import AdventOfCode.Prelude
import Control.Monad (unless, when)
import Control.Monad.ST (runST)
import Data.ByteString qualified as BS
import Data.Char qualified as Char
import Data.Int (Int8)
import Data.Massiv.Array qualified as M
import Data.Massiv.Array.Unsafe (unsafeFreeze)
import Data.Massiv.Core (Ix2 (..))
import Data.Monoid (Sum (..))
import Data.Word (Word8)

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

c2w :: Char -> Word8
c2w = fromIntegral . Char.ord

solve :: M.Array M.U Ix2 Int8 -> (Int, Int)
solve grid = (countRemoved ns, solve2 ns)
  where
    ns = M.compute @M.U $ neighboursM grid

countRemoved :: (M.Index ix, M.Source r Int8) => M.Array r ix Int8 -> Int
countRemoved = getSum . M.foldMono (\i -> if i < 4 then Sum 1 else Sum 0)

solve2 :: (M.Manifest r Int8) => M.Array r Ix2 Int8 -> Int
solve2 grid = countRemoved $ M.withMArrayST_ grid $ \mg -> do
  let go !ix = do
        n <- M.readM mg ix
        when (n >= 0 && n < 4) $ do
          M.writeM mg ix (-1)
          for (-1) 1 $ \dr ->
            for (-1) 1 $ \dc ->
              unless (dr == 0 && dc == 0) $ do
                let !next = ix + (dr :. dc)
                M.read mg next >>= \case
                  Nothing -> pure ()
                  Just m
                    | m < 0 || m >= 100 -> pure ()
                    | otherwise -> do
                        M.modify_ mg (\x -> pure $ x - 1) next
                        go next

  M.iforM_ grid $ \ix i -> when (i < 4) $ go ix
  pure ()

neighboursM :: (M.Manifest r Int8) => M.Array r Ix2 Int8 -> M.Array M.DW Ix2 Int8
neighboursM = M.mapStencil (M.Fill 0) stencil
  where
    stencil = M.makeStencil (M.Sz (3 :. 3)) (1 :. 1) $ \get ->
      get (-1 :. -1)
        + get (-1 :. 0)
        + get (-1 :. 1)
        + get (0 :. -1)
        + (1 - get (0 :. 0)) * 100
        + get (0 :. 1)
        + get (1 :. -1)
        + get (1 :. 0)
        + get (1 :. 1)
