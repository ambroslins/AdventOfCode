module AdventOfCode.Day20 (solution) where

import AdventOfCode.Parser qualified as Parser
import AdventOfCode.Prelude
import Control.Exception (assert)
import Control.Monad (void)
import Data.Char (isAsciiLower)
import Data.HashMap.Lazy qualified as HashMap
import Data.HashMap.Strict ((!))
import Data.HashSet qualified as HashSet
import Data.List (find, findIndex)
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
      part2 = uncurry solve2
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
    (lows, highs) = foldl' sumPair (0, 0) $ take 1001 states
    states = iteratePush broadcasts modules
    sumPair (!x1, !y1) (!x2, y2, _) = (x1 + x2, y1 + y2)

solve2 :: [Name] -> HashMap Name Module -> Int
solve2 broadcasts modules = counts ! output
  where
    states = map (\(_, _, ons) -> ons) $ iteratePush broadcasts modules
    output =
      maybe
        (error "no output module")
        fst
        $ find (elem "rx" . destinations . snd)
        $ HashMap.toList modules
    counts = HashMap.mapWithKey f modules
    f name Module {ty = t, inputs} = case t of
      FlipFlop ->
        fromMaybe (error "no loop") $
          findIndex (HashSet.member name) states
      Conjunction
        | all (== FlipFlop) inTypes -> sum inCounts
        | all (== Conjunction) inTypes -> foldl' lcm 1 inCounts
        | otherwise -> error "not all inputs are the same type"
        where
          inTypes = map ty $ mapMaybe (`HashMap.lookup` modules) inputs
          inCounts = mapMaybe (`HashMap.lookup` counts) inputs

iteratePush :: [Name] -> HashMap Name Module -> [(Int, Int, HashSet Name)]
iteratePush broadcasts modules = go (0, 0, HashSet.empty)
  where
    go state@(_, _, ons) =
      state : go (push broadcasts modules ons)

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
