module ParseUtils
  ( integer
  , endOfLine
  , trimSpacesEOF
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

integer :: ReadP Int
integer = read <$> positive +++ negative
  where
    positive = munch1 isDigit
    negative = char '-' `parseAndAppend` positive

trimSpacesEOF :: ReadP a -> ReadP a
trimSpacesEOF parser = skipSpaces *> parser <* skipSpaces <* eof
