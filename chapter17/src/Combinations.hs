module Combinations where

import Control.Applicative (liftA3)

stops = "pbtdkg"

vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos = liftA3 combo

combos' :: [a] -> [b] -> [c] -> [(a, b, c)]
combos' as bs cs = [combo] <*> as <*> bs <*> cs

combo :: a -> b -> c -> (a, b, c)
combo = (,,)