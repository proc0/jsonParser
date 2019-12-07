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

_null     :: String
_true     :: String
_false    :: String
_char     :: String
_delimit  :: String
_brace    :: String
_bracket  :: String
_empty    :: String
_keyword  :: String
_invalid  :: String
_no_match :: String
_no_ints  :: String

_null     = "null"
_true     = "true"
_false    = "false"
_char     = "Char: "
_delimit  = "Delimitter: "
_brace    = "Object: "
_bracket  = "Array: "
_empty    = "Empty"
_keyword  = "Keyword: "
_invalid  = "Invalid: "
_no_match = "No Match"
_no_ints  = "No Integers"