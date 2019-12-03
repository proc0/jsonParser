module Main where

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

readJson :: FilePath -> IO String
readJson path = do
    Right result <- parseFile path json
    return $ show result

filepath = "./test.json"

main :: IO String
main = do
    readJson filepath
