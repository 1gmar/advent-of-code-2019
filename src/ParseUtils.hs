module ParseUtils
  ( integer
  , integerStr
  , endOfLine
  , trimSpacesEOF
  , parseAndAppend
  , module ParserCombinators
  ) where

import           Data.Char                    (isControl, isDigit)
import           Text.ParserCombinators.ReadP as ParserCombinators (ReadP, char, choice, count, eof, many1, munch1,
                                                                    readP_to_S, satisfy, sepBy, skipSpaces, string,
                                                                    (+++))

endOfLine :: ReadP Char
endOfLine = satisfy isControl

parseAndAppend :: ReadP a -> ReadP [a] -> ReadP [a]
parseAndAppend parser list = parser >>= \token -> (token :) <$> list

integerStr :: ReadP String
integerStr = positive +++ negative
  where
    positive = munch1 isDigit
    negative = char '-' `parseAndAppend` positive

integer :: ReadP Int
integer = read <$> integerStr

trimSpacesEOF :: ReadP a -> ReadP a
trimSpacesEOF parser = skipSpaces *> parser <* skipSpaces <* eof
