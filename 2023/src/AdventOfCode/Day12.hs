module AdventOfCode.Day12 (solution) where

import AdventOfCode.Parser qualified as Parser
import AdventOfCode.Prelude
import Control.Monad.ST (ST, runST)
import Control.Parallel.Strategies (parMap, rseq)
import Data.ByteString.Char8 qualified as BS
import Data.Vector.Unboxed (MVector, Vector)
import Data.Vector.Unboxed qualified as Vector
import Data.Vector.Unboxed.Mutable qualified as MVector

solution :: Solution
solution =
  Solution
    { parser = parseLine `sepEndBy` Parser.endOfLine,
      solver = solve1 &&& solve2
    }

solve1 :: [(ByteString, Vector Int)] -> Int
solve1 = sum . map (uncurry arrangements)

solve2 :: [(ByteString, Vector Int)] -> Int
solve2 = sum . parMap rseq (uncurry arrangements . unfold)

unfold :: (ByteString, Vector Int) -> (ByteString, Vector Int)
unfold (springs, groups) =
  ( BS.intercalate "?" (replicate 5 springs),
    Vector.concat (replicate 5 groups)
  )

parseLine :: Parser (ByteString, Vector Int)
parseLine = do
  springs <- Parser.takeWhile1 (/= ' ') <* Parser.space
  groups <- Parser.decimal `sepEndBy'` Parser.char ','
  pure (springs, Vector.fromList groups)

arrangements :: ByteString -> Vector Int -> Int
arrangements springs groups = runST $ do
  seen <- MVector.replicate (BS.length springs * (Vector.length groups + 1)) (-1)
  go seen springs groups
  where
    memo :: MVector s Int -> ByteString -> Vector Int -> ST s Int
    memo seen s gs = do
      n <- MVector.read seen key
      if n >= 0
        then pure n
        else do
          m <- go seen s gs
          MVector.write seen key m
          pure m
      where
        key = BS.length s + Vector.length gs * BS.length springs
    go :: MVector s Int -> ByteString -> Vector Int -> ST s Int
    go seen s grps = case Vector.uncons grps of
      Nothing -> pure $ if '#' `BS.elem` s then 0 else 1
      Just (g, gs) -> case BS.uncons $ BS.dropWhile (== '.') s of
        Just ('#', rest) -> closeGroup rest
        Just ('?', rest) -> do
          n1 <- closeGroup rest
          n2 <- memo seen rest grps
          pure (n1 + n2)
        _ -> pure 0
        where
          closeGroup rest
            | BS.length rest < g - 1 || '.' `BS.elem` damaged = pure 0
            | otherwise = case BS.uncons operational of
                Nothing | Vector.null gs -> pure 1
                Just (x, xs) | x /= '#' -> memo seen xs gs
                _ -> pure 0
            where
              (damaged, operational) = BS.splitAt (g - 1) rest
