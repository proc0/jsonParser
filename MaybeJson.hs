module MaybeJson (Parser, runParser, Result, json) where

import Control.Applicative 
import Data.Function
import Data.Char
import Data.Tuple
import Alias

-- JSON
    
json :: Parser Value
json = element

element :: Parser Value
element = the value

value :: Parser Value 
value = object
    <|> array 
    <|> string
    <|> number 
    <|> bool 
    <|> nu11

-- spec

data Value 
    = OBJECT [(String, Value)]
    | ARRAY [Value]
    | STRING String 
    | NUMBER Integer 
    | BOOL Bool 
    | NULL
    deriving (Show, Eq)

object :: Parser Value
object = OBJECT <$> grammar 
    where grammar = left brace *> the members <* right brace
          members = delimit (the comma) member            
          member = pair <$> the str <*> the colon <*> element
          pair = (\name _ value -> (name, value))  

array :: Parser Value
array = ARRAY <$> grammar 
    where grammar = left bracket *> the elements <* right bracket
          elements = delimit (the comma) element               

string :: Parser Value
string = STRING <$> str

number :: Parser Value
number = NUMBER . read <$> digits
    where digits = guard result
          result = group isDigit

bool :: Parser Value
bool = choose <$> choice
    where choice = true <|> false
          choose bool = BOOL $ 
            case bool of
                "true"  -> True 
                "false" -> False

nu11 :: Parser Value
nu11 = void <$> token "null"
                     
void :: a -> Value
void _ = NULL

-- grammar

true :: Parser String
true = token "true"

false :: Parser String
false = token "false"

quote :: Parser Char
quote = character '"'

unqoute :: Parser String
unqoute = group $ (/=) '"'

str :: Parser String
str = quote *> unqoute <* quote

ws :: Parser String
ws = group isSpace

comma :: Parser Char
comma = character ','

colon :: Parser Char
colon = character ':'

brace :: (Parser Char, Parser Char)
brace = couple "{}"

bracket :: (Parser Char, Parser Char)
bracket = couple "[]"

-- parsers

the :: Parser a -> Parser a
the parse = ws *> parse <* ws

token :: String -> Parser String
token = sequenceA . map character

couple :: String -> (Parser Char, Parser Char) 
couple pair = parsePair (head pair, last pair)
    where parsePair = uncurry ((,) `on` character)

character :: Char -> Parser Char
character ch = Parser char
    where char [] = Nothing
          char (c:cs)
            | c == ch = Just (cs, ch)
            | otherwise = Nothing

guard :: Parser [a] -> Parser [a]
guard (Parser match) = Parser $ 
    \input -> do
        (scrap, result) <- match input
        if not (null result)
        then Just (scrap, result)
        else Nothing

delimit :: Parser a -> Parser b -> Parser [b]
delimit delimitter match = (:) <$> 
    match <*> group <|> pure []
        where group = many $ delimitter *> match

group :: (Char -> Bool) -> Parser String
group filter = Parser $ -- (result, rest)
    \input -> Just $ swap (span filter input)

-- Parser
----------

type Scrap = String

newtype Parser result = Parser 
    { 
        runParser :: String -> Maybe (Scrap, result) 
    }

instance Functor Parser where                       -- Parser is a functor since
    fmap rematch (Parser match) = Parser $          -- given a transform and a Parser
        \input -> do                                -- it returns a Parser that
            (scrap, result) <- match input          -- parses its input and returns
            Just (scrap, rematch result)            -- the transformed result with the rest.

instance Applicative Parser where                   -- Parser is an Applicative since
    pure result = Parser $                          -- given a result it returns a Parser  
        \input -> Just (input, result)              -- that returns its input and that result,
    Parser prematch <*> Parser match = Parser $     -- and given two Parsers it returns a Parser
        \input -> do                                -- 
            (rest, rematch) <- prematch input       -- that uses the first parser on the input,
            (scrap, result) <- match rest           -- pipes the rest to the second parser,
            Just (scrap, rematch result)            -- and composes their results.

instance Alternative Parser where
    empty = Parser $ 
        \_ -> Nothing
    Parser match <|> Parser rematch = Parser $ 
        \input -> match input <|> rematch input

