module EitherJson (
    Parser, 
    Result, 
    decode, 
    json
) where

import Control.Applicative 
import Data.Char
import Data.Function
import Data.Tuple

import Alias
import Parser

{- JSON -}

json  :: Parser Value
value :: Parser Value 

json = element
value = object  OBJECT
    <|> array   ARRAY
    <|> string  >$< STRING
    <|> number  NUMBER
    <|> bool    BOOL
    <|> void    NULL

object      :: ([(String, Value)] -> Value) -> Parser Value
array       :: ([Value] -> Value) -> Parser Value
element     :: Parser Value

object      = (>$<) rule where 
    rule    = left brace *> ws members <* right brace
    members = delimit member $ ws comma          
    member  = couple <$> ws string <*> ws colon <*> element

array       = (>$<) rule where
    rule    = left bracket *> ws elements <* right bracket
    elements = delimit element $ ws comma

element = ws value

number  :: (Integer -> Value) -> Parser Value
bool    :: (Bool -> Value) -> Parser Value
void    :: Value -> Parser Value

number  = (<$> integers) . (. read)

bool    = (<$> match) . (. key) where
            match = true <|> false
            key word 
              | word == _TRUE  = True
              | word == _FALSE = False

void    = (<$> keyword _NULL) . const 

ws :: Parser a -> Parser a
ws token = whitespace *> token <* whitespace

data Value 
    = OBJECT [(String, Value)]
    | ARRAY  [Value]
    | STRING String 
    | NUMBER Integer 
    | BOOL   Bool 
    | NULL
    deriving (Show, Eq)

string      :: Parser String
characters  :: Parser String
whitespace  :: Parser String
true        :: Parser String
false       :: Parser String
integers    :: Parser String

string      = quote *> characters <* quote
characters  = group $ (/=) '"'
whitespace  = group isSpace
true        = keyword _TRUE
false       = keyword _FALSE
integers    = guard digits where
    digits  = group isDigit

quote       :: Parser Char
comma       :: Parser Char
colon       :: Parser Char
brace       :: (Parser Char, Parser Char)
bracket     :: (Parser Char, Parser Char)

quote       = character '"'
comma       = character ','
colon       = character ':'
brace       = pair      "{}"
bracket     = pair      "[]"

keyword     :: String -> Parser String
pair        :: String -> (Parser Char, Parser Char)
character   :: Char -> Parser Char
delimit     :: Parser a -> Parser b -> Parser [a]
group       :: (Char -> Bool) -> Parser String
guard       :: Parser [a] -> Parser [a]

keyword = sequenceA . map character

-- constructs tuple of parsers for enclosures
pair (start:end:[]) = parse (start, end) where
    parse = uncurry ((,) `on` character)

-- primitive recursive parser
character ch = Parser char where 
    char [] = Left _LEFT_CHR_MSG
    char (c:cs)
        | c == ch = Right (cs, ch)
        | otherwise = Left _LEFT_STR_MSG

-- constructs parser of lists from a list of parsers
delimit match delimitter = (:) <$> 
    match <*> group <|> pure []
        where group = many $ delimitter *> match

-- constructs a parser that groups by character
group filter = Parser $ -- (result, rest)
    \input -> Right $ swap (span filter input)

-- constructs a parser that guards a parser
guard (Parser match) = Parser $ 
    \input -> do
        (source, result) <- match input
        if not (null result)
        then Right (source, result)
        else Left _LEFT_GRD_MSG
