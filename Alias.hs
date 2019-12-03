module Alias where

left = fst
right = snd

couple :: a -> b -> c -> (a, c)
couple = (\name _ value -> (name, value))

(>$<) :: (Functor f) => f a -> (a -> b) -> f b
(>$<) = flip fmap

_TRUE :: String
_FALSE :: String
_NULL :: String
_LEFT_CHR_MSG :: String
_LEFT_STR_MSG :: String
_LEFT_GRD_MSG :: String

_TRUE   = "true"
_FALSE  = "false"
_NULL   = "null"
_LEFT_CHR_MSG = "char finish"
_LEFT_STR_MSG = "string finish"
_LEFT_GRD_MSG = "guard finish"