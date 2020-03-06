module Util.ParseUtils
  ( integer
  , endOfLine
  , trimSpacesEOF
  , parseInput
  , module ParserCombinators
  ) where

import           Data.Char                    (isControl, isDigit)
import           Text.ParserCombinators.ReadP as ParserCombinators (ReadP, char, choice, count, eof, many1, munch1,
                                                                    optional, readP_to_S, satisfy, sepBy, skipSpaces,
                                                                    string, (+++))

endOfLine :: ReadP Char
endOfLine = satisfy isControl

parsePrepend :: ReadP a -> ReadP [a] -> ReadP [a]
parsePrepend parser list = parser >>= \token -> (token :) <$> list

integer :: ReadP Int
integer = read <$> positive +++ negative
  where
    positive = munch1 isDigit
    negative = char '-' `parsePrepend` positive

trimSpacesEOF :: ReadP a -> ReadP a
trimSpacesEOF parser = skipSpaces *> parser <* skipSpaces <* eof

parseInput :: ReadP [a] -> String -> [a]
parseInput inputParser = concatMap fst . readP_to_S inputParser
