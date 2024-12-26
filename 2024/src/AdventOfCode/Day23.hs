module AdventOfCode.Day23 (solution) where

import AdventOfCode.Parser qualified as Parser
import AdventOfCode.Prelude
import Data.Bits (setBit, testBit)
import Data.ByteString.Char8 qualified as BS
import Data.Char qualified as Char
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.List qualified as List

type Graph = IntMap IntSet

solution :: Solution
solution =
  Solution
    { parser = parseConnection `sepEndBy` Parser.endOfLine,
      solver = solve
    }

parseConnection :: Parser (ByteString, ByteString)
parseConnection = do
  a <- Parser.take 2
  Parser.symbol "-"
  b <- Parser.take 2
  pure (a, b)

solve :: [(ByteString, ByteString)] -> (Int, ByteString)
solve connections = (solve1 edges, solve2 edges)
  where
    edges =
      IntMap.fromListWith (<>) $
        concat
          [ [(ha, IntSet.singleton hb), (hb, IntSet.singleton ha)]
          | (a, b) <- connections,
            let ha = hash a,
            let hb = hash b
          ]

solve1 :: Graph -> Int
solve1 edges = part1
  where
    Pair part1 _ = IntMap.foldlWithKey' go (Pair @Int @Int 0 0) edges
    go (Pair acc seen) n1 neighbours
      | n1 < ta || n1 > tz = Pair acc seen
      | otherwise =
          let triangles =
                sum
                  [ 1
                  | n2 : ns <- List.tails $ IntSet.toList neighbours,
                    n3 <- ns,
                    n2 < ta || n2 > tz || not (seen `testBit` (n2 - ta)),
                    n3 < ta || n3 > tz || not (seen `testBit` (n3 - ta)),
                    maybe False (IntSet.member n3) $ IntMap.lookup n2 edges
                  ]
           in Pair (acc + triangles) (seen `setBit` (n1 - ta))

-- https://en.wikipedia.org/wiki/Bron-Kerbosch_algorithm#With_pivoting
solve2 :: Graph -> ByteString
solve2 edges =
  BS.intercalate "," $
    map unhash $
      IntSet.toList $
        List.maximumBy (compare `on` IntSet.size) $
          go IntSet.empty (IntMap.keysSet edges) IntSet.empty
  where
    go !r !p !x
      | IntSet.null p && IntSet.null x = [r]
      | otherwise =
          let u = head $ IntSet.toList p <> IntSet.toList x
              nu = IntMap.findWithDefault IntSet.empty u edges
              step (p', x') v =
                let nv = IntMap.findWithDefault IntSet.empty v edges
                 in ( (IntSet.delete v p', IntSet.insert v x'),
                      go
                        (IntSet.insert v r)
                        (IntSet.intersection p' nv)
                        (IntSet.intersection x' nv)
                    )
           in concat $
                snd $
                  List.mapAccumL step (p, x) $
                    IntSet.elems $
                      p IntSet.\\ nu

hash :: ByteString -> Int
hash = BS.foldl' (\acc c -> acc * 26 + Char.ord c - Char.ord 'a') 0

ta, tz :: Int
ta = hash "ta"
tz = hash "tz"

unhash :: Int -> ByteString
unhash h =
  let (q, r) = h `quotRem` 26
   in BS.pack [Char.chr (q + Char.ord 'a'), Char.chr (r + Char.ord 'a')]
