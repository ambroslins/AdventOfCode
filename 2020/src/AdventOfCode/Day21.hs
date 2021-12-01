module AdventOfCode.Day21 where

import AdventOfCode.Prelude
import Data.List (intercalate)
import qualified Data.Map as Map
import qualified Data.Set as Set

solution :: Solution
solution = Solution (parseFood `sepEndBy1` eol) solve1 solve2

type Ingredient = String

type Allergen = String

type Food = ([String], Set Allergen)

parseFood :: Parser Food
parseFood = do
  is <- some lowerChar `sepEndBy1` hspace1
  as <- between (char '(') (char ')') $ symbol "contains" >> some lowerChar `sepEndBy1` symbol ","
  pure (is, Set.fromList as)

solve1 :: [Food] -> Int
solve1 xs = sum $ map (length . filter (`Set.notMember` is) . fst) xs
  where
    is = Set.fromList $ Map.elems $ isolate xs

solve2 :: [Food] -> String
solve2 = intercalate "," . Map.elems . isolate

isolate :: [Food] -> Map Allergen Ingredient
isolate xs = Set.elemAt 0 <$> until (all ((== 1) . length)) f asMap
  where
    asMap =
      Map.fromSet
        (\k -> foldl1 Set.intersection $ map (Set.fromList . fst) (filter (Set.member k . snd) xs))
        $ Set.unions $ map snd xs
    f m =
      Map.foldlWithKey
        ( \as k is -> case Set.toList is of
            [x] -> Map.mapWithKey (\k' -> if k /= k' then Set.delete x else id) as
            _ -> as
        )
        m
        m
