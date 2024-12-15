{-# LANGUAGE ApplicativeDo #-}

module AdventOfCode.Day15 (solution) where

import AdventOfCode.Grid (Grid)
import AdventOfCode.Grid qualified as Grid
import AdventOfCode.Parser qualified as Parser
import AdventOfCode.Position qualified as Pos
import AdventOfCode.Prelude
import AdventOfCode.Search qualified as Search
import Control.Monad (foldM, forM_)
import Control.Monad.ST.Strict (runST)
import Data.Bool (bool)
import Data.Vector.Unboxed qualified as VU

solution :: Solution
solution =
  Solution
    { parser = do
        grid <- Grid.parse
        Parser.endOfLine
        moves <- Parser.many' parseMove
        Parser.endOfLine
        pure (grid, moves),
      solver = uncurry solve &&& uncurry (solve . widen)
    }

parseMove :: Parser Direction
parseMove =
  Parser.anyChar >>= \case
    '^' -> pure North
    '>' -> pure East
    'v' -> pure South
    '<' -> pure West
    '\n' -> parseMove
    c -> fail $ "unexpected move: " <> show c

solve :: Grid VU.Vector Char -> [Direction] -> Int
solve grid moves = runST $ do
  tiles <- Grid.thaw grid
  Grid.write tiles start '.'

  let findEmpty !pos !dir =
        let next = Pos.move dir pos
         in Grid.read tiles next >>= \case
              '.' -> pure $ Just next
              '#' -> pure Nothing
              'O' -> findEmpty next dir
              t -> error $ "solve: unexpected tile: " <> show t

  let push !dir ps = do
        ts <- Grid.unsafeFreeze tiles
        let next p =
              let p' = Pos.move dir p
               in case Grid.unsafeIndex ts p' of
                    '#' -> []
                    '.' -> []
                    '[' -> [p', Pos.move East p']
                    ']' -> [p', Pos.move West p']
                    t -> error $ "solve: unexpected tile: " <> show t
        let boxes = Search.bfsOnInt hash next ps
        if any (\p -> Grid.unsafeIndex ts (Pos.move dir p) == '#') boxes
          then pure False
          else do
            forM_ (reverse boxes) $ \p -> Grid.swap tiles p (Pos.move dir p)
            pure True
  let move !pos !dir = do
        let next = Pos.move dir pos
        Grid.read tiles next >>= \case
          '.' -> pure next
          '#' -> pure pos
          'O' ->
            findEmpty next dir >>= \case
              Nothing -> pure pos
              Just p -> Grid.swap tiles p next $> next
          '[' -> bool pos next <$> push dir [next, Pos.move East next]
          ']' -> bool pos next <$> push dir [next, Pos.move West next]
          t -> error $ "solve: unexpected tile: " <> show t
  _ <- foldM move start moves
  boxes <- Grid.findPositions (\c -> c == 'O' || c == '[') <$> Grid.unsafeFreeze tiles
  pure $ VU.foldl' (\acc (Position r c) -> acc + 100 * r + c) 0 boxes
  where
    !start =
      fromMaybe (error "solve: no start") $
        Grid.findPosition (== '@') grid
    !ncols = Grid.ncols grid
    hash (Position r c) = r * ncols + c

widen :: Grid VU.Vector Char -> Grid VU.Vector Char
widen grid = Grid.create $ do
  wide <- Grid.newMutable nrows (2 * ncols) '.'
  forM_ [0 .. nrows - 1] $ \r -> forM_ [0 .. ncols - 1] $ \c ->
    case Grid.unsafeIndex grid (Position r c) of
      '#' -> do
        Grid.write wide (Position r (2 * c)) '#'
        Grid.write wide (Position r (2 * c + 1)) '#'
      'O' -> do
        Grid.write wide (Position r (2 * c)) '['
        Grid.write wide (Position r (2 * c + 1)) ']'
      '@' -> Grid.write wide (Position r (2 * c)) '@'
      _ -> pure ()
  pure wide
  where
    (!nrows, !ncols) = Grid.size grid

{-
display :: Grid VU.Vector Char -> Grid VU.Vector Char
display grid = trace (go $ Grid.rows grid) grid
  where
    go = unlines . map VU.toList
-}
