module AdventOfCode.Day15 (solution) where

import AdventOfCode.Parser qualified as Parser
import AdventOfCode.Prelude
import Control.Monad.ST.Strict (ST)
import Data.ByteString.Char8 qualified as BS
import Data.Char (isAsciiLower, ord)
import Data.Foldable (traverse_)
import Data.Vector (MVector, Vector)
import Data.Vector qualified as Vector
import Data.Vector.Mutable qualified as MVector

type Label = ByteString

type Lens = (Label, Int)

type Box = [Lens]

data Operation = Delete !Label | Insert !Label !Int
  deriving (Eq, Show)

solution :: Solution
solution =
  Solution
    { parser = parseOperation `sepEndBy'` Parser.char ',' <* Parser.endOfLine,
      part1 = solve1,
      part2 = solve2
    }

solve1 :: [Operation] -> Int
solve1 = sum . map (hash . label)

solve2 :: [Operation] -> Int
solve2 operations = focusingPower $ Vector.create $ do
  boxes <- MVector.replicate 256 []
  traverse_ (apply boxes) operations
  pure boxes

parseOperation :: Parser Operation
parseOperation = do
  !l <- Parser.takeWhile1 isAsciiLower
  (Delete l <$ Parser.char '-')
    <|> (Insert l <$> (Parser.char '=' *> Parser.decimal))

label :: Operation -> Label
label = \case
  Delete l -> l
  Insert l _ -> l

hash :: Label -> Int
hash = BS.foldl' (\value c -> ((value + ord c) * 17) `mod` 256) 0

focusingPower :: Vector Box -> Int
focusingPower = Vector.sum . Vector.imap box
  where
    box n lenses =
      sum $
        zipWith (\i (_, l) -> (n + 1) * i * l) [1 ..] lenses

apply :: MVector s Box -> Operation -> ST s ()
apply v = \case
  Delete l -> MVector.write v (hash l) []
  Insert l focalLength -> MVector.modify v (insert l focalLength) (hash l)

insert :: Label -> Int -> Box -> Box
insert l focalLength = go
  where
    go = \case
      [] -> [(l, focalLength)]
      x@(l', _) : xs
        | l == l' -> (l, focalLength) : xs
        | otherwise -> x : go xs
