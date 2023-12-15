module AdventOfCode.Day15 (solution) where

import AdventOfCode.Parser qualified as Parser
import AdventOfCode.Prelude
import Data.ByteString.Char8 qualified as BS
import Data.Char (isAlphaNum, isAsciiLower, ord)
import Data.HashMap.Strict qualified as HashMap

type Lens = (ByteString, Int)

type Box = [Lens]

solution :: Solution
solution =
  Solution
    { parser = initalization `sepEndBy` Parser.char ',' <* Parser.endOfLine,
      part1 = solve1,
      part2 = solve2
    }

solve1 :: [ByteString] -> Int
solve1 = sum . map hash

solve2 :: [ByteString] -> Int
solve2 = focusingPower . foldl' (flip step) HashMap.empty

initalization :: Parser ByteString
initalization =
  Parser.takeWhile1 (\c -> isAlphaNum c || c == '-' || c == '=')

hash :: ByteString -> Int
hash = BS.foldl' go 0
  where
    go value c = ((value + ord c) * 17) `mod` 256

focusingPower :: HashMap Int Box -> Int
focusingPower = sum . map box . HashMap.toList
  where
    box (k, v) = sum $ zipWith (\i (_, l) -> (k + 1) * i * l) [1 ..] v

step :: ByteString -> HashMap Int Box -> HashMap Int Box
step s = HashMap.alter operation (hash label)
  where
    (label, rest) = BS.span isAsciiLower s
    operation = case BS.uncons rest of
      Just ('-', _) -> \case
        Nothing -> Nothing
        Just xs ->
          let ys = filter ((/= label) . fst) xs
           in if null ys then Nothing else pure ys
      Just ('=', focalLength) ->
        case BS.readInt focalLength of
          Nothing -> error "invalid focal length"
          Just (f, _) -> \case
            Nothing -> pure [(label, f)]
            Just xs -> pure $ insert label f xs
      _ -> error "invalid operation"

insert :: ByteString -> Int -> Box -> Box
insert label focalLength = \case
  [] -> [(label, focalLength)]
  x@(l, _) : xs
    | l == label -> (l, focalLength) : xs
    | otherwise -> x : insert label focalLength xs
