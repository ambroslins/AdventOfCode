{-# LANGUAGE GADTs #-}

module AdventOfCode.Prelude
  ( module Text.Megaparsec,
    module Text.Megaparsec.Char,
    module Control.Monad.Combinators,
    module Data.Maybe,
    module Data.Vector,
    Day (..),
    Map,
    Set,
    Solution (..),
    Text,
    Text.pack,
    Text.unpack,
    Parser,
    asum,
    find,
    sc,
    sc',
    signed,
    simpleSolution,
    lexeme,
    lexeme',
    symbol,
    Lexer.decimal,
    Lexer.hexadecimal,
    Lexer.float,
  )
where

import Control.Monad.Combinators
  ( between,
    choice,
    count,
    empty,
    endBy,
    endBy1,
    many,
    manyTill,
    option,
    optional,
    sepBy,
    sepBy1,
    sepEndBy,
    sepEndBy1,
    skipMany,
    skipSome,
    some,
    someTill,
    (<|>),
  )
import Data.Foldable (asum, find)
import Data.Map (Map)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Vector (Vector, (!?))
import Data.Void (Void)
import Text.Megaparsec
  ( Parsec,
    match,
    noneOf,
    notFollowedBy,
    oneOf,
    satisfy,
    try,
    unexpected,
  )
import qualified Text.Megaparsec as Megaparsec
import Text.Megaparsec.Char
  ( alphaNumChar,
    char,
    digitChar,
    eol,
    hspace,
    hspace1,
    letterChar,
    lowerChar,
    space,
    space1,
    spaceChar,
    string,
    tab,
    upperChar,
  )
import qualified Text.Megaparsec.Char.Lexer as Lexer

type Parser = Parsec Void Text

newtype Day = Day Int
  deriving (Eq, Ord, Show)

data Solution where
  Solution :: (Show b, Show c) => Parser a -> (a -> b) -> (a -> c) -> Solution

simpleSolution :: (Show b, Show c) => (String -> a) -> (a -> b) -> (a -> c) -> Solution
simpleSolution p = Solution (p . Text.unpack <$> Megaparsec.takeRest)

sc :: Parser ()
sc = Lexer.space hspace1 empty empty

sc' :: Parser ()
sc' = Lexer.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme sc

lexeme' :: Parser a -> Parser a
lexeme' = Lexer.lexeme sc'

symbol :: Text -> Parser Text
symbol = Lexer.symbol sc

signed :: Num a => Parser a -> Parser a
signed = Lexer.signed sc
