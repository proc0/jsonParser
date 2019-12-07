module Main where

import Data.Either
import Text.Pretty.Simple (pPrint)
import EitherJson
import Alias

parseFile :: 
    FilePath 
 -> Parser json 
 -> IO (Result json)
parseFile path parser = do
    file <- readFile path
    let result = decode parser file
    return result

readJson :: FilePath -> IO ()
readJson path = do
    output <- parseFile path json
    case output of
        Right (source, result) -> pPrint result
        Left stack -> pPrint stack
        
filepath = "./tests/mock/invalid2.json"

main :: IO ()
main = do
    readJson filepath
