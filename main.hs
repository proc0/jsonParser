{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative 
import Data.Function
import Data.Char
import Data.Tuple

left = fst
right = snd

main :: IO String
main = do
    fromJson "./test.json" 

fromJson :: FilePath -> IO String
fromJson filepath = do
    result <- parseFile filepath token
    return $ show result

parseFile :: FilePath -> Parser a -> IO (Maybe a)
parseFile filepath parser = do
    file <- readFile filepath
    return $ right <$> runParser parser file

--

-- specification
-------------------

-- primitive types

void :: a -> Json
void _ = Null

-- null parser
none :: Parser Json
none = void <$> nu11

-- true | false
bool :: Parser Json
bool = choose <$> choice
    where choice = true <|> false
          choose bit = Bit $ 
            case bit of
                "true" -> True 
                "false" -> False

int :: Parser Json
int = Int . read <$> digits
    where digits = maybeParse result
          result = splitParse isDigit

str :: Parser Json
str = Str <$> string

-- recursive types

the :: Parser a -> Parser a
the parse = space *> parse <* space

array :: Parser Json
array = Array <$> spec 
    where spec = left bracket *> the entries <* right bracket   -- [ entries ]
          entries = parseGroup (the comma) token                -- [ token, token, token ]

object :: Parser Json
object = Object <$> spec 
    where spec = left brace *> the entries <* right brace       -- { entries }
          entries = parseGroup (the comma) entry                -- { entry, entry, entry }
          entry = pair <$> string <*> the colon <*> token       -- { string : token, ... }
          pair = (\key _ value -> (key, value))                 -- ( string, token )

token :: Parser Json 
token = none 
    <|> bool 
    <|> int 
    <|> str 
    <|> array 
    <|> object

--

-- keywords
-----------


nu11 :: Parser String
nu11 = parseString "null"

true :: Parser String
true = parseString "true"

false :: Parser String
false = parseString "false"

quote :: Parser Char
quote = parseChar '"'

space :: Parser String
space = splitParse isSpace

unqoute :: Parser String
unqoute = splitParse $ (/=) '"'

-- TODO: add escape support
string :: Parser String
string = quote *> unqoute <* quote

comma :: Parser Char
comma = parseChar ','

colon :: Parser Char
colon = parseChar ':'

brace :: (Parser Char, Parser Char)
brace = parsePair "{}"

bracket :: (Parser Char, Parser Char)
bracket = parsePair "[]"


--

-- combinators
--------------

parseChar :: Char -> Parser Char
parseChar ch = Parser char
    where char [] = Nothing
          char (c:cs)
            | c == ch = Just (cs, ch)
            | otherwise = Nothing

parseString :: String -> Parser String
parseString = sequenceA . map parseChar

maybeParse :: Parser [a] -> Parser [a]
maybeParse (Parser parse) = Parser $ 
    \input -> do
        (rest, result) <- parse input
        if not (null result)
        then Just (rest, result)
        else Nothing

splitParse :: (Char -> Bool) -> Parser String
splitParse filter = Parser $ -- (result, rest)
    \input -> Just $ swap (span filter input)

parsePair :: String -> (Parser Char, Parser Char) 
parsePair pair = parsePair (head pair, last pair)
    where parsePair = uncurry ((,) `on` parseChar)

parseGroup :: Parser a -> Parser b -> Parser [b]
parseGroup parser parsing = 
    (:) <$> parsing <*> parsers <|> pure []
        where parsers = many (parser *> parsing)

-- definitions
--------------

newtype Parser a = Parser 
    { 
        runParser :: String -> Maybe (String, a) 
    }

data Json 
    = Null
    | Bit Bool 
    -- TODO: add Float
    | Int Integer 
    | Str String 
    | Array [Json]
    | Object [(String, Json)]
    deriving (Show, Eq)

instance Functor Parser where
    fmap transform (Parser parse) = Parser $ 
        \input -> do
            (rest, result) <- parse input
            Just (rest, transform result)

instance Applicative Parser where
    pure result = Parser $ 
        \input -> Just (input, result)
    Parser parse <*> Parser parse' = Parser $ 
        \input -> do
            (output, transform) <- parse input
            (rest, result) <- parse' output
            Just (rest, transform result)

instance Alternative Parser where
    empty = Parser $ 
        \_ -> Nothing
    Parser parse <|> Parser parse' = Parser $ 
        \input -> parse input <|> parse' input

