module AdventOfCode.Parser
  ( module Data.Attoparsec.ByteString.Char8,
    Alternative (..),
    int,
    lexeme,
    line,
    list,
    symbol,
    whitespace,
    sepEndBy',
  )
where

import Control.Applicative (Alternative (..))
import Data.Attoparsec.ByteString qualified as Word8
import Data.Attoparsec.ByteString.Char8
import Data.ByteString (ByteString)

int :: Parser Int
int = signed decimal

line :: Parser ByteString
line = Word8.takeWhile1 (not . isEndOfLine) <* endOfLine

list :: Parser a -> Parser [a]
list p = p `sepBy` lexeme (char ',')

whitespace :: Parser ()
whitespace = Word8.skipWhile isHorizontalSpace

lexeme :: Parser a -> Parser a
lexeme p = p <* whitespace

symbol :: ByteString -> Parser ()
symbol s = string s *> whitespace

sepEndBy' :: Parser a -> Parser b -> Parser [a]
sepEndBy' p sep = sepBy' p sep <* option undefined sep
