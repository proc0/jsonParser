import Control.Applicative 
import Data.Function
import Data.Char
import Data.Tuple


left = fst
right = snd

main :: IO String
main = do
    readJson "./test.json" 

readJson :: FilePath -> IO String
readJson path = do
    result <- parseFile path json
    return $ case result of
        Right result -> show $ right result
        Left failure -> "Invalid JSON"

parseFile :: FilePath -> Parser json -> IO (Result json)
parseFile path parser = do
    file <- readFile path
    let result = runParser parser file
    return result

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
brace = pair "{}"

bracket :: (Parser Char, Parser Char)
bracket = pair "[]"

-- parsers

the :: Parser a -> Parser a
the parse = ws *> parse <* ws

token :: String -> Parser String
token = sequenceA . map character

pair :: String -> (Parser Char, Parser Char)
pair (left:right:[]) = couple (left, right)
    where couple = uncurry ((,) `on` character)
pair _ = (empty, empty)

character :: Char -> Parser Char
character ch = Parser char
    where char [] = Left "Done"
          char (c:cs)
            | c == ch = Right (cs, ch)
            | otherwise = Left "No match: character"

guard :: Parser [a] -> Parser [a]
guard (Parser match) = Parser $ 
    \input -> do
        (source, result) <- match input
        if not (null result)
        then Right (source, result)
        else Left "No match: number"

delimit :: Parser a -> Parser b -> Parser [b]
delimit delimitter match = (:) <$> 
    match <*> group <|> pure []
        where group = many $ delimitter *> match

group :: (Char -> Bool) -> Parser String
group filter = Parser $ -- (result, rest)
    \input -> Right $ swap (span filter input)

-- Parser
----------

type Source = String
type Failure = String
type Result match = Either Failure (Source, match)

newtype Parser json = Parser
    { 
        runParser :: Source -> Result json
    }

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
            (partial, connect) <- process input
            (source, result) <- parse partial
            return (source, connect result)

instance Alternative Parser where
    empty = Parser $ 
        \_ -> Left "Empty"
    Parser process <|> Parser parse = Parser $ 
        \input -> case process input of
            Right result -> Right result
            Left failure -> parse input

