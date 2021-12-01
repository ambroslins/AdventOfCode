module AdventOfCode.Day04 where

import AdventOfCode.Prelude
import Data.Char (isDigit)
import qualified Data.Map as Map

type Passport = Map String String

parsePassport :: Parser Passport
parsePassport =
  Map.fromList
    <$> some
      ( do
          key <- count 3 letterChar
          _ <- char ':'
          value <- some (alphaNumChar <|> char '#')
          _ <- spaceChar
          pure (key, value)
      )

solution :: Solution
solution =
  Solution
    (parsePassport `sepBy` eol)
    (length . filter isValid1)
    (length . filter isValid2)

isValid1 :: Passport -> Bool
isValid1 p = all (`Map.member` p) ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

inRange :: Int -> Int -> Int -> Bool
inRange a b x = a <= x && x <= b

isValid2 :: Passport -> Bool
isValid2 p =
  all
    (\(k, f) -> maybe False f $ Map.lookup k p)
    [ ("byr", \v -> length v == 4 && inRange 1920 2002 (read v)),
      ("iyr", \v -> length v == 4 && inRange 2010 2020 (read v)),
      ("eyr", \v -> length v == 4 && inRange 2020 2030 (read v)),
      ( "hgt",
        \v -> case span isDigit v of
          (x, "cm") -> inRange 150 193 (read x)
          (x, "in") -> inRange 59 76 (read x)
          _ -> False
      ),
      ( "hcl",
        \case
          '#' : v -> length v == 6 && all (\x -> isDigit x || x `elem` ['a' .. 'f']) v
          _ -> False
      ),
      ("ecl", flip elem ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]),
      ("pid", \v -> length v == 9 && all isDigit v)
    ]
