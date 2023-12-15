module AdventOfCode.Day12 (solution) where

import AdventOfCode.Parser qualified as Parser
import AdventOfCode.Prelude
import Control.Monad.State.Strict (State)
import Control.Monad.State.Strict qualified as State
import Control.Parallel.Strategies (parMap, rseq)
import Data.ByteString.Char8 qualified as BS
import Data.HashMap.Strict qualified as HashMap
import Data.Vector.Unboxed (Vector)
import Data.Vector.Unboxed qualified as Vector

type Cache = HashMap Int Int

solution :: Solution
solution =
  Solution
    { parser = parseLine `sepEndBy` Parser.endOfLine,
      part1 = solve1,
      part2 = solve2
    }

parseLine :: Parser (ByteString, Vector Int)
parseLine = do
  springs <- Parser.takeWhile1 (/= ' ') <* Parser.space
  groups <- Parser.decimal `sepEndBy'` Parser.char ','
  pure (springs, Vector.fromList groups)

arrangements :: ByteString -> Vector Int -> Int
arrangements bs =
  flip State.evalState HashMap.empty . go bs
  where
    memo :: ByteString -> Vector Int -> State Cache Int
    memo springs groups =
      State.gets (HashMap.lookup key) >>= \case
        Just n -> pure n
        Nothing -> do
          n <- go springs groups
          State.modify (HashMap.insert key n)
          pure n
      where
        key = BS.length springs + Vector.length groups * (BS.length bs + 1)
    go :: ByteString -> Vector Int -> State Cache Int
    go springs groups =
      case Vector.uncons groups of
        Nothing -> pure $ if '#' `BS.elem` springs then 0 else 1
        Just (g, gs) -> case BS.uncons $ BS.dropWhile (== '.') springs of
          Just ('#', rest) -> closeGroup rest
          Just ('?', rest) -> do
            n1 <- closeGroup rest
            n2 <- memo rest groups
            pure (n1 + n2)
          _ -> pure 0
          where
            closeGroup rest
              | BS.length rest < g - 1 || '.' `BS.elem` damaged = pure 0
              | otherwise = case BS.uncons operational of
                  Nothing | Vector.null gs -> pure 1
                  Just (x, xs) | x /= '#' -> memo xs gs
                  _ -> pure 0
              where
                (damaged, operational) = BS.splitAt (g - 1) rest

solve1 :: [(ByteString, Vector Int)] -> Int
solve1 = sum . map (uncurry arrangements)

solve2 :: [(ByteString, Vector Int)] -> Int
solve2 = sum . parMap rseq (uncurry arrangements . unfold)

unfold :: (ByteString, Vector Int) -> (ByteString, Vector Int)
unfold (springs, groups) =
  ( BS.intercalate "?" (replicate 5 springs),
    Vector.concat (replicate 5 groups)
  )
