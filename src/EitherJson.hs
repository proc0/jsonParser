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

json  = element
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
    rule    = left brace *> members <* right brace
    members = delimit member comma          
    member  = touple <$> ws string <*> colon <*> element

array       = (>$<) rule where
    rule    = left bracket *> elements <* right bracket
    elements = delimit element $ ws comma

element = ws value

number  :: (Integer -> Value) -> Parser Value
bool    :: (Bool -> Value) -> Parser Value
void    :: Value -> Parser Value

number  = (<$> integers) . (. read)

bool    = (<$> match) . (. key) where
            match = true <|> false
            key word 
              | word == _true  = True
              | word == _false = False

void    = (<$> keyword _null) . const 

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
characters  = sparse $ (/=) '"'
whitespace  = sparse isSpace
true        = keyword _true
false       = keyword _false
integers    = guard _no_ints digits where
    digits  = sparse isDigit

quote       :: Parser Char
comma       :: Parser Char
colon       :: Parser Char
brace       :: (Parser Char, Parser Char)
bracket     :: (Parser Char, Parser Char)

quote       = character _delimit '"'
comma       = character _delimit ','
colon       = character _delimit ':'
brace       = pair _brace "{}"
bracket     = pair _bracket "[]"

keyword     :: String -> Parser String
pair        :: String -> String -> (Parser Char, Parser Char)
character   :: String -> Char -> Parser Char
delimit     :: Parser a -> Parser b -> Parser [a]
sparse      :: (Char -> Bool) -> Parser String
guard       :: String -> Parser [a] -> Parser [a]

keyword = sequenceA . map (character _keyword)

-- constructs tuple of parsers for enclosures
pair label (start:end:[]) = parse (start, end) where
    parse = uncurry ((,) `on` (character label))

-- primitive recursive parser
character label ch = Parser char where 
    char [] = Left []
    char (c:cs)
        | c == ch = Right (cs, ch)
        | otherwise = Left [Left $ label ++ [ch]]

-- constructs parser of lists from a list of parsers
delimit match delimitter = (:) <$> 
    match <*> parse <|> pure []
        where parse = many $ delimitter *> match

-- constructs a parser that groups by character
sparse filter = Parser $ -- (result, rest)
    \input -> Right $ swap (span filter input)

-- constructs a parser that guards a parser
guard label (Parser match) = Parser $ 
    \input -> do
        (source, result) <- match input
        if not (null result)
        then Right (source, result)
        else Left [Left label]
