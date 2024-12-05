module AdventOfCode.Day05 (solution) where

import AdventOfCode.Parser qualified as Parser
import AdventOfCode.Prelude
import Control.Monad (forM_)
import Control.Monad.ST.Strict (ST, runST)
import Data.Coerce (coerce)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.List.NonEmpty qualified as NonEmpty
import Data.Monoid (Sum (..))
import Data.STRef.Strict (STRef, newSTRef, readSTRef, writeSTRef)
import Data.Vector.Unboxed (Vector)
import Data.Vector.Unboxed qualified as Vector
import Data.Vector.Unboxed.Mutable qualified as MVector

data Rule = Rule !Int !Int
  deriving (Eq, Show)

type Update = Vector Int

solution :: Solution
solution =
  Solution
    { parser = do
        rules <- parseRule `sepEndBy'` Parser.endOfLine
        Parser.endOfLine
        updates <- parseUpdate `sepEndBy'` Parser.endOfLine
        pure (rules, updates),
      solver = uncurry solve
    }

parseRule :: Parser Rule
parseRule = do
  x <- Parser.decimal
  _ <- Parser.char '|'
  y <- Parser.decimal
  pure $ Rule x y

parseUpdate :: Parser Update
parseUpdate = (Vector.fromList . NonEmpty.toList) <$> (Parser.decimal `sepBy1` Parser.char ',')

solve :: [Rule] -> [Update] -> (Int, Int)
solve rules updates = coerce $ foldMap' go updates
  where
    go update =
      let (modified, mid) =
            partialSort
              (\i -> IntMap.findWithDefault IntSet.empty i gt)
              update
       in if modified then (0, Sum mid) else (Sum mid, 0)
    gt =
      IntMap.fromListWith
        (<>)
        [(y, IntSet.singleton x) | Rule x y <- rules]

partialSort :: (Int -> IntSet) -> Vector Int -> (Bool, Int)
partialSort gt updates = runST $ do
  modified <- newSTRef False :: ST s (STRef s Bool)
  v <- Vector.thaw updates

  forM_ [0 .. MVector.length v - 1] $ fix $ \loop i -> do
    x <- MVector.read v i
    v' <- Vector.unsafeFreeze (MVector.drop (i + 1) v)
    case Vector.findIndex (`IntSet.member` gt x) v' of
      Nothing -> pure ()
      Just j -> do
        writeSTRef modified True
        MVector.swap v i (i + 1 + j)
        loop i

  m <- readSTRef modified
  middle <- MVector.read v (MVector.length v `div` 2)
  pure (m, middle)
