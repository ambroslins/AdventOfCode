module AdventOfCode.Day23 (solution) where

import AdventOfCode.Parser qualified as Parser
import AdventOfCode.Prelude
import Data.ByteString.Char8 qualified as BS
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Debug.Trace (traceWith)

solution :: Solution
solution =
  Solution
    { parser = parseConnection `sepEndBy` Parser.endOfLine,
      solver = solve1 &&& solve2
    }

parseConnection :: Parser (ByteString, ByteString)
parseConnection = do
  a <- Parser.take 2
  Parser.symbol "-"
  b <- Parser.take 2
  pure (a, b)

solve1 :: [(ByteString, ByteString)] -> Int
solve1 connections =
  Set.size $
    Set.fromList $
      [ List.sort [a, b, c]
      | a <- Set.toList tNodes,
        let as = Map.lookup a edges,
        b <- maybe [] Set.toList as,
        c <- maybe [] Set.toList $ Map.lookup b edges,
        maybe False (Set.member c) as
      ]
  where
    tNodes =
      Set.fromList $
        concatMap
          (\(a, b) -> [x | x <- [a, b], "t" `BS.isPrefixOf` x])
          connections
    edges =
      Map.unionsWith (<>) $
        map
          ( \(a, b) ->
              let ha = a
                  hb = b
               in Map.fromList [(ha, Set.singleton hb), (hb, Set.singleton ha)]
          )
          connections

solve2 :: [(ByteString, ByteString)] -> Int
solve2 connections =
  Set.size
    $ traceWith (BS.unpack . BS.intercalate "," . Set.toAscList)
    $ List.maximumBy (compare `on` Set.size)
    $ filter
      fullyConnected
    $ concatMap
      ( \n ->
          map (Set.insert n . Set.fromAscList) $
            maybe [] (List.subsequences . Set.toAscList) $
              Map.lookup n edges
      )
    $ Map.keys edges
  where
    edges =
      Map.unionsWith (<>) $
        [ Map.fromList [(a, Set.singleton b), (b, Set.singleton a)]
        | (a, b) <- connections
        ]
    fullyConnected s =
      all (\n -> maybe False (Set.isSubsetOf (Set.delete n s)) $ Map.lookup n edges) s
