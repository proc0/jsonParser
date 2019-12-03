module Alias where

left = fst
right = snd

couple :: a -> b -> c -> (a, c)
couple = (\name _ value -> (name, value))

(>$<) :: (Functor f) => f a -> (a -> b) -> f b
(>$<) = flip fmap
