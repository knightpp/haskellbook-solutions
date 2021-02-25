module VariationsOnEither where

import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

import Test.QuickCheck(Arbitrary(arbitrary))
import Test.QuickCheck.Gen (oneof)

data Validation e a
  = Failure e
  | Success a
  deriving (Eq, Show)

instance Functor (Validation e) where
  fmap _ (Failure e) = Failure e
  fmap f (Success a) = Success (f a)

instance Monoid e => Applicative (Validation e) where
  pure = Success
  (Failure e) <*> (Failure e') = Failure (e <> e')
  (Failure e) <*> _ = Failure e
  _ <*> (Failure e) = Failure e
  (Success f) <*> (Success x) = Success (f x)

instance (Arbitrary e, Arbitrary a) => Arbitrary (Validation e a) where
  arbitrary = do
    e <- arbitrary
    a <- arbitrary 
    oneof [return e, return a]
instance (Eq e, Eq a) => EqProp (Validation e a) where
  (=-=) = eq

tests = do
  -- quickBatch $ functor (undefined::Validation [String] (Int, String, Integer))
  quickBatch $ applicative (undefined::Validation [String] (Int, String, Integer))

-- af <*> x = case af of
--   (Success f) -> case x of
--     (Success x) -> Success (f x)
--     (Failure e) -> Failure e
--   (Failure e) -> case x of
--     (Success x) -> Failure e
--     (Failure e') -> Failure (e <> e')