module Alias where

import Data.List
import Data.Maybe

left = fst
right = snd

couple :: a -> b -> c -> (a, c)
couple = (\name _ value -> (name, value))

(>$<) :: Functor f => f a -> (a -> b) -> f b
(>$<) = flip fmap

duplicate :: String -> (String, String)
duplicate label = fromJust $ (head <$>) <$> uncons (replicate 2 label)

_TRUE     :: String
_FALSE    :: String
_NULL     :: String
_CHAR     :: String
_TOKEN    :: String
_BRACE    :: String
_BRACKET  :: String
_EMPTY    :: String
_KEYWORD  :: String
_INVALID  :: String
_NO_MATCH :: String
_NO_INTS  :: String

_NULL     = "null"
_TRUE     = "true"
_FALSE    = "false"
_CHAR     = "Char: "
_TOKEN    = "Token: "
_BRACE    = "Object: "
_BRACKET  = "Array: "
_EMPTY    = "Empty"
_KEYWORD  = "Keyword: "
_INVALID  = "Invalid: "
_NO_MATCH = "No Match"
_NO_INTS  = "No Integers"