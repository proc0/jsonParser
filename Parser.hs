module Parser where

import Control.Applicative 

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