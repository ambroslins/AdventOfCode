{-# LANGUAGE MultiWayIf #-}

module AdventOfCode.Day08 (solution) where

import AdventOfCode.Parser qualified as Parser
import AdventOfCode.Prelude
import Control.Monad (when)
import Control.Monad.ST.Strict (runST)
import Control.Scheduler (trivialScheduler_)
import Data.ByteString qualified as BS
import Data.Massiv.Array (Ix, Ix1, Ix2 (..), Ix3, IxN (Ix3), Sz (..), unSz)
import Data.Massiv.Array qualified as M
import Data.Massiv.Array.Mutable.Algorithms (quicksortByM_)
import Data.Massiv.Array.Unsafe (unsafeFreeze)
import Debug.Trace (traceShow, traceShowId)

solution :: Solution
solution =
  Solution
    { parser = M.fromList M.Seq <$> (parseLine `sepEndBy'` Parser.endOfLine),
      solver = solve
    }

parseLine :: Parser Ix3
parseLine = do
  x <- Parser.decimal
  _ <- Parser.char ','
  y <- Parser.decimal
  _ <- Parser.char ','
  z <- Parser.decimal
  pure $! Ix3 x y z

solve :: M.Vector M.U Ix3 -> (Int, Int)
solve positions =
  runST $ do
    graphs <- M.newMArray @M.U (M.size positions) (-1)
    let connect !g !i = do
          let Ix2 i1 i2 = M.index' pairs i
          g1 <- M.readM graphs i1
          g2 <- M.readM graphs i2
          if
            | g1 < 0 && g2 < 0 -> do
                M.writeM graphs i1 g
                M.writeM graphs i2 g
                pure (g + 1)
            | g2 < 0 -> do
                M.writeM graphs i2 g1
                pure g
            | g1 < 0 -> do
                M.writeM graphs i1 g2
                pure g
            | otherwise -> do
                when (g1 /= g2) $ M.forPrimM graphs (\g' -> pure $ if g' == g2 then g1 else g')
                pure g
    ngraphs <- M.foldlM connect 0 $ M.take (Sz steps) sortedPairs
    counts <- M.newMArray @M.U @Int (Sz ngraphs + 1) 0
    M.forPrimM_ graphs (\g -> when (g >= 0) $ M.modifyM_ counts (\x -> pure $ x + 1) g)
    quicksortByM_ (\x y -> pure $ compare y x) trivialScheduler_ counts
    part1 <- M.product . M.take 3 <$> unsafeFreeze M.Seq counts

    let goUntilConnected !i !g = do
          g0 <- M.readM graphs 0
          connected <- M.all (== g0) <$> unsafeFreeze M.Seq graphs
          if connected then pure i else connect g (M.index' sortedPairs (i + 1)) >>= goUntilConnected (i + 1)

    connector <- goUntilConnected (steps - 1) ngraphs
    let (Ix2 i j) = M.index' pairs (M.index' sortedPairs connector)
        (Ix3 x1 _ _) = M.index' positions i
        (Ix3 x2 _ _) = M.index' positions j
        !part2 = x1 * x2

    pure (part1, part2)
  where
    steps = if size > 20 then 1000 else 10
    size = M.unSz $ M.size positions
    pairs =
      let n = size * (size - 1) `div` 2
       in M.compute @M.U $
            M.iterateN
              (Sz n)
              (\(Ix2 i j) -> let j' = j + 1 in if j' == size then Ix2 (i + 1) (i + 2) else Ix2 i j')
              (Ix2 0 0)
    distances = M.compute @M.U $ M.map (\(Ix2 i j) -> distanceSquared (M.index' positions i) (M.index' positions j)) pairs
    sortedPairs = M.quicksortBy (compare `on` M.index' distances) $ M.compute @M.U $ M.range M.Seq 0 (unSz $ M.size pairs)

distanceSquared :: Ix3 -> Ix3 -> Int
distanceSquared (Ix3 x1 y1 z1) (Ix3 x2 y2 z2) = square (x2 - x1) + square (y2 - y1) + square (z2 - z1)
  where
    square !x = x * x
