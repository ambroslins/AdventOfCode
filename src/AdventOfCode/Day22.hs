module AdventOfCode.Day22 where

import AdventOfCode.Prelude
import Data.Foldable (Foldable (toList))
import Data.Sequence
  ( Seq,
    ViewL (EmptyL, (:<)),
    (|>),
  )
import qualified Data.Sequence as Seq
import qualified Data.Set as Set

solution :: Solution
solution = Solution parseInput solve1 solve2

parseInput :: Parser (Seq Int, Seq Int)
parseInput = do
  _ <- lexeme' $ string "Player 1:"
  xs <- decimal `sepEndBy1` eol
  space
  _ <- lexeme' $ string "Player 2:"
  ys <- decimal `sepEndBy1` eol
  pure (Seq.fromList xs, Seq.fromList ys)

combat :: Seq Int -> Seq Int -> Seq Int
combat p1 p2 = case (Seq.viewl p1, Seq.viewl p2) of
  (EmptyL, _) -> p2
  (_, EmptyL) -> p1
  (x :< xs, y :< ys) -> case compare x y of
    GT -> combat (xs |> x |> y) ys
    LT -> combat xs (ys |> y |> x)
    EQ -> error "equal elements"

solve1 :: (Seq Int, Seq Int) -> Int
solve1 = sum . zipWith (*) [1 ..] . reverse . toList . uncurry combat

recursiveCombat :: Seq Int -> Seq Int -> Either (Seq Int) (Seq Int)
recursiveCombat = go Set.empty
  where
    go previous deck1 deck2 =
      if (deck1, deck2) `Set.member` previous
        then Left deck1
        else case (Seq.viewl deck1, Seq.viewl deck2) of
          (EmptyL, _) -> Right deck2
          (_, EmptyL) -> Left deck1
          (x :< xs, y :< ys) ->
            let winner =
                  if Seq.length xs >= x && Seq.length ys >= y
                    then recursiveCombat (Seq.take x xs) (Seq.take y ys)
                    else case compare x y of
                      GT -> Left xs
                      LT -> Right ys
                      EQ -> error "equal elements"
                set' = Set.insert (deck1, deck2) previous
             in case winner of
                  Left _ -> go set' (xs |> x |> y) ys
                  Right _ -> go set' xs (ys |> y |> x)

solve2 :: (Seq Int, Seq Int) -> Int
solve2 = sum . zipWith (*) [1 ..] . reverse . toList . either id id . uncurry recursiveCombat