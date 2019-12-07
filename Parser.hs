module Parser where

import Control.Applicative 
import Alias

newtype Parser match = Parser
    { 
        decode :: Source -> Result match
    }

type Source = String
type Stack = [Either String Source]
type Result match = Either Stack (Source, match)

instance Functor Parser where                       
    fmap process (Parser parse) = Parser $          
        \input -> do                                
            (source, output) <- parse input                   
            return (source, process output)    

instance Applicative Parser where                   
    pure result = Parser $                           
        \input -> Right (input, result)
    Parser process <*> Parser parse = Parser $
        \input -> case process input of
            Right (partial, compose) -> case parse partial of
                Right (source, result) -> Right (source, compose result)
                Left stack -> Left $ stack ++ [Left $ _invalid ++ partial]
            Left stack -> Left stack

instance Alternative Parser where
    empty = Parser $ 
        \_ -> Left []
    Parser process <|> Parser parse = Parser $ 
        \input -> case process input of
            Right proceed -> Right proceed
            Left stack -> case parse input of
                Left end -> Left $ stack ++ [Left _no_match] ++ end
                Right result -> Right result
