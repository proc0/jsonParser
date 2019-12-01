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

{- JSON -}

{- McKeeman Form
   crockford.com/mckeeman.html

json
    element

value
    object
    array
    string
    number
    "true"
    "false"
    "null" 
-}

json :: Parser Value
json = element

value :: Parser Value 
value = object
    <|> array 
    <|> string
    <|> number 
    <|> bool 
    <|> nu11

-- carrier
data Value 
    = OBJECT [(String, Value)]
    | ARRAY  [Value]
    | STRING String 
    | NUMBER Integer 
    | BOOL   Bool 
    | NULL
    deriving (Show, Eq)

{-
object
    '{' ws '}'
    '{' members '}'
members
    member
    member ',' members
member
    ws string ws ':' element
array
    '[' ws ']'
    '[' elements ']'
elements
    element
    element ',' elements
element
    ws value ws -}

object :: Parser Value
object = OBJECT <$> rule where 
    rule = left brace *> ws members <* right brace
    members = delimit (ws comma) member            
    member = pair <$> ws literal <*> ws colon <*> element
    pair = (\name _ value -> (name, value))  

array :: Parser Value
array = ARRAY <$> rule where
    rule = left bracket *> ws elements <* right bracket
    elements = delimit (ws comma) element               

element :: Parser Value
element = ws value

{- 
string
    '"' characters '"'
nothing
    ""
    indentation '"' '"' newline
characters
    ""
    character characters
escape
    '"'
    '\'
    '/'
    'b'
    'f'
    'n'
    'r'
    't'
    'u' hex hex hex hex
hex
    digit
    'A' . 'F'
    'a' . 'f'
ws
    ""
    '0020' ws
    '000A' ws
    '000D' ws
    '0009' ws -}

string :: Parser Value
string = STRING <$> literal

literal :: Parser String
literal = quote *> characters <* quote

characters :: Parser String
characters = group $ (/=) '"'

whitespace :: Parser String
whitespace = group isSpace

-- with spaces (i.e. ' <...> ')
ws :: Parser a -> Parser a
ws parser = whitespace *> parser <* whitespace

{- 
number
    integer fraction exponent
integer 
    digit 
    onenine digits
    '-' digit
    '-' onenine digits
digits
    digit
    digit digits
digit
    '0'
    onenine
onenine
    '1' . '9'
exponent
    ""
    'E' sign digits
    'e' sign digits
sign
    ""
    '+' -}
-- primitive rescursive token
number :: Parser Value
number = NUMBER . read <$> integers
    where integers = guard digits
          digits = group isDigit

bool :: Parser Value
bool = choose <$> choice where 
    choice = true <|> false
    choose bool = BOOL $ 
        case bool of
            "true"  -> True 
            "false" -> False

nu11 :: Parser Value
nu11 = void <$> keyword "null"
                     
void :: a -> Value
void _ = NULL

true :: Parser String
true = keyword "true"

false :: Parser String
false = keyword "false"

quote :: Parser Char
quote = character '"'

comma :: Parser Char
comma = character ','

colon :: Parser Char
colon = character ':'

brace :: (Parser Char, Parser Char)
brace = pair "{}"

bracket :: (Parser Char, Parser Char)
bracket = pair "[]"

{- Parsers -}

keyword :: String -> Parser String
keyword = sequenceA . map character

-- constructs tuple of parsers for enclosures
pair :: String -> (Parser Char, Parser Char)
pair (left:right:[]) = couple (left, right)
    where couple = uncurry ((,) `on` character)

-- constructs parser of lists from a list of parsers
delimit :: Parser a -> Parser b -> Parser [b]
delimit delimitter match = (:) <$> 
    match <*> group <|> pure []
        where group = many $ delimitter *> match

-- constructs a parser that groups by character
group :: (Char -> Bool) -> Parser String
group filter = Parser $ -- (result, rest)
    \input -> Right $ swap (span filter input)

{- character
    '0020' . '10FFFF' - '"' - '\'
    '\' escape -}
-- primitive rescursive token
character :: Char -> Parser Char
character ch = Parser char where 
    char [] = Left "Done"
    char (c:cs)
        | c == ch = Right (cs, ch)
        | otherwise = Left "No match: character"

-- constructs a parser that guards a parser
guard :: Parser [a] -> Parser [a]
guard (Parser match) = Parser $ 
    \input -> do
        (source, result) <- match input
        if not (null result)
        then Right (source, result)
        else Left "No match"

{- Parser -}

newtype Parser value = Parser
    { 
        decode :: Source -> Result value
    }

type Source = String
type Invalid = String
type Result value = Either Invalid (Source, value)

instance Functor Parser where                       
    fmap process (Parser parse) = Parser $          
        \input -> do                                
            (source, output) <- parse input                   
            return (source, process output)    

instance Applicative Parser where                   
    pure result = Parser $                           
        \input -> Right (input, result)
    Parser process <*> Parser parse = Parser $
        \input -> do
            (partial, compose) <- process input
            (source, result) <- parse partial
            return (source, compose result)

instance Alternative Parser where
    empty = Parser $ 
        \_ -> Left "Empty"
    Parser process <|> Parser parse = Parser $ 
        \input -> case process input of
            Right result -> Right result
            Left failure -> parse input

