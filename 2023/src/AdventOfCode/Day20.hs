module AdventOfCode.Day20 (solution) where

import AdventOfCode.Parser qualified as Parser
import AdventOfCode.Prelude
import Control.Exception (assert)
import Control.Monad (void)
import Data.Char (isAsciiLower)
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Deque.Lazy qualified as Deque
import GHC.IsList (fromList)

type Name = ByteString

data Type = FlipFlop | Conjunction
  deriving (Eq, Show)

data Module = Module
  { ty :: !Type,
    inputs :: [Name],
    destinations :: [Name]
  }
  deriving (Show)

data Pulse = Low | High
  deriving (Eq, Show)

solution :: Solution
solution =
  Solution
    { parser = buildInput <$> parseModule `sepEndBy` Parser.endOfLine,
      part1 = uncurry solve1,
      part2 = length
    }

parseName :: Parser Name
parseName = Parser.takeWhile1 isAsciiLower

parseModule :: Parser (Maybe Type, Name, [Name])
parseModule = do
  m <- optional $ flipflop <|> conjunction
  name <- parseName
  void $ Parser.string " -> "
  dests <- parseName `sepBy` ", "
  pure (m, name, dests)
  where
    flipflop = FlipFlop <$ Parser.char '%'
    conjunction = Conjunction <$ Parser.char '&'

buildInput :: [(Maybe Type, Name, [Name])] -> ([Name], HashMap Name Module)
buildInput xs = (broadcaster, HashMap.fromList modules)
  where
    (broadcaster, modules) = foldr go (error "no broadcaster", []) xs
    go (m, name, destinations) (b, ms) =
      case m of
        Nothing -> assert (name == "broadcaster") (destinations, ms)
        Just ty ->
          let inputs = mapMaybe (inDestinations name) modules
           in (b, (name, Module {ty, inputs, destinations}) : ms)
    inDestinations name (n, Module {destinations})
      | name `elem` destinations = Just n
      | otherwise = Nothing

solve1 :: [Name] -> HashMap Name Module -> Int
solve1 broadcasts modules = lows * highs
  where
    (lows, highs) = foldl' sumPair (0, 0) $ take 1000 states
    states = iteratePush broadcasts modules
    sumPair (!x1, !y1) (!x2, y2) = (x1 + x2, y1 + y2)

iteratePush :: [Name] -> HashMap Name Module -> [(Int, Int)]
iteratePush broadcasts modules = go (push broadcasts modules HashSet.empty)
  where
    go (lows, highs, ons) =
      (lows, highs) : go (push broadcasts modules ons)

push :: [Name] -> HashMap Name Module -> HashSet Name -> (Int, Int, HashSet Name)
push broadcasts modules = go (1, 0) (fromList $ map (,Low) broadcasts)
  where
    go (!lows, !highs) queue ons = case Deque.uncons queue of
      Nothing -> (lows, highs, ons)
      Just ((name, pulse), q) -> case HashMap.lookup name modules of
        Nothing -> go pulses q ons
        Just Module {ty, inputs, destinations} -> case ty of
          FlipFlop
            | pulse == High -> go pulses q ons
            | name `HashSet.member` ons -> send Low
            | otherwise -> send High
          Conjunction
            | pulse == High && all (`HashSet.member` ons) inputs -> send Low
            | otherwise -> send High
          where
            send p =
              go pulses (q <> fromList (map (,p) destinations)) $ case p of
                Low -> HashSet.delete name ons
                High -> HashSet.insert name ons
        where
          pulses = case pulse of
            Low -> (lows + 1, highs)
            High -> (lows, highs + 1)
