module AdventOfCode.Parser
  ( module Data.Attoparsec.ByteString.Char8,
    Alternative (..),
    line,
  )
where

import Control.Applicative (Alternative (..))
import Data.Attoparsec.ByteString qualified as Word8
import Data.Attoparsec.ByteString.Char8
import Data.ByteString (ByteString)

line :: Parser ByteString
line = Word8.takeWhile1 (not . isEndOfLine) <* endOfLine
