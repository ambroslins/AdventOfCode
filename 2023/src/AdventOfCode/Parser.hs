module AdventOfCode.Parser
  ( module Data.Attoparsec.ByteString.Char8,
    Alternative (..),
    runParser,
    int,
    lexeme,
    line,
    lines,
    symbol,
    whitespace,
  )
where

import Control.Applicative (Alternative (..))
import Data.Attoparsec.ByteString qualified as Word8
import Data.Attoparsec.ByteString.Char8
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS
import Prelude hiding (lines)

runParser :: Parser a -> ByteString -> a
runParser p bs = case parseOnly (p <* endOfInput) bs of
  Left err -> error $ "runParser: " <> err
  Right x -> x

int :: Parser Int
int = signed decimal

line :: Parser ByteString
line = Word8.takeWhile1 (not . isEndOfLine)

lines :: Parser [ByteString]
lines = BS.lines <$> takeByteString

whitespace :: Parser ()
whitespace = Word8.skipWhile isHorizontalSpace

lexeme :: Parser a -> Parser a
lexeme p = p <* whitespace

symbol :: ByteString -> Parser ()
symbol s = string s *> whitespace
