module AdventOfCode.Day16 where

import AdventOfCode.Prelude
import Data.List (isPrefixOf)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Vector as Vector

type Rule = (String, Int -> Bool)

type Rules = Map String (Int -> Bool)

type Ticket = Vector Int

data Input = Input Rules Ticket [Ticket]

parseRule :: Parser Rule
parseRule = do
  name <- manyTill (alphaNumChar <|> char ' ') (char ':') <* hspace
  let range = do
        lower <- decimal
        _ <- char '-'
        upper <- decimal
        pure $ \x -> lower <= x && x <= upper
  p1 <- range
  _ <- string " or "
  p2 <- range
  pure (name, \x -> p1 x || p2 x)

parseTicket :: Parser Ticket
parseTicket = Vector.fromList <$> decimal `sepBy1` char ','

parseInput :: Parser Input
parseInput = do
  rs <- Map.fromList <$> parseRule `sepEndBy` eol
  space
  string "your ticket:" >> space
  your <- parseTicket
  space
  string "nearby tickets:" >> space
  Input rs your <$> parseTicket `sepEndBy` eol

solution :: Solution
solution = Solution parseInput solve1 solve2

solve1 :: Input -> Int
solve1 (Input rules _ nearby) = sum $ filter (\x -> not $ any ($ x) rules) $ concatMap Vector.toList nearby

solve2 :: Input -> Int
solve2 (Input rules ticket nearby) = product $ Vector.ifilter (\i -> const (isPrefixOf "departure" $ classes Vector.! i)) ticket
  where
    valid = filter (all $ \x -> any ($ x) rules) nearby
    field i = Map.keysSet $ foldr ((\x -> Map.filter ($ x)) . (Vector.! i)) rules valid
    fields = Vector.imap (\i _ -> field i) ticket
    unify xs =
      Vector.ifoldr
        ( \i s -> case Set.toList s of
            [x] -> Vector.imap $ \i' -> if i /= i' then Set.delete x else id
            _ -> id
        )
        xs
        xs
    classes = Vector.map (head . Set.toList) $ until (all ((== 1) . Set.size)) unify fields
