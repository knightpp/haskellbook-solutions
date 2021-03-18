module LibraryFunctions where

import Control.Monad
import Data.Functor.Identity
import Data.Maybe
import Data.Monoid

--  task 1
sum' ::
  (Foldable t, Num a) =>
  t a ->
  a
-- sum' = foldr (+) 0
sum' = getSum . foldMap Sum

-- task 2
product' ::
  (Foldable t, Num a) =>
  t a ->
  a
product' = getProduct . foldMap Product

-- task 3
elem' :: (Foldable t, Eq a) => a -> t a -> Maybe a
-- elem' e t =
--   getFirst $ foldMap f t
--   where
--     f a =
--       if a == e
--         then First $ Just a
--         else First Nothing
elem' e =
  foldr
    (\a b -> if a == e then Just a else b)
    Nothing

-- | task 4
minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' = foldr task4 Nothing

task4 :: Ord a => a -> Maybe a -> Maybe a
--task4 a ma = Just $ maybe a (min a) ma
task4 a ma = Just $ maybe a (min a) ma

-- | task 5
maximum' :: (Foldable t, Ord a) => t a -> Maybe a
maximum' = foldr (\a ma -> Just $ maybe a (max a) ma) Nothing

-- | task 6
null' :: (Foldable t) => t a -> Bool
-- null' = foldr (\_ _ -> False) True
null' ta = foldMap (\_ -> Sum 1) ta == 0

-- | task 7
length' :: (Foldable t) => t a -> Int
-- length' ta = getSum $ foldMap (\_ -> Sum 1) ta
length' = foldr (\_ acc -> acc + 1) 0 

-- | task 8
toList' :: (Foldable t) => t a -> [a]
-- | toList' = foldMap (: [])
toList' = foldr (:) []
-- | task 9
fold'::(Foldable t, Monoid m) => t m -> m
-- fold' = foldMap id 
fold' = foldr (<>) mempty 
-- | task 10
foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f = foldr (\a acc -> f a <> acc) mempty  


