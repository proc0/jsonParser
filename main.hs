module Main where

import EitherJson
import Alias

filepath = "./test.json"

parseFile :: 
    FilePath 
 -> Parser json 
 -> IO (Result json)
parseFile path parser = do
    file <- readFile path
    let result = decode parser file
    return result
--  MaybeJson diff:
--  return $ right <$> result

readJson :: FilePath -> IO String
readJson path = do
    result <- parseFile path json
    return $ show result

main :: IO String
main = do
    readJson filepath
