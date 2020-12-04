module Parsing
( module Parsing
, module Text.Parsec
) where

import Text.Parsec
import MUtil

type Parser = Parsec String ()

runParse :: Parser a -> String -> Either ParseError a
runParse = flip parse ""

expectParse :: Parser a -> String -> a
expectParse = (fromRight .) . runParse

-- Some combinators / parsers

optionalSpace :: Parser ()
optionalSpace = (() <$ space) <|> eof

number :: Parser Int
number = read <$> many1 digit

word :: Parser String
word = manyTill anyChar optionalSpace
