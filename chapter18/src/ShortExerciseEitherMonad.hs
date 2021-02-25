module ShortExerciseEitherMonad where

import Test.QuickCheck (Arbitrary (arbitrary), oneof)
import Test.QuickCheck.Checkers (EqProp (..), eq, quickBatch)
import Test.QuickCheck.Classes (applicative, functor, monad)

data Sum a b
  = First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap f x = case x of
    First a -> First a
    Second b -> Second $ f b

instance Applicative (Sum a) where
  pure = Second
  (First a) <*> _ = First a
  _ <*> (First a) = First a
  (Second f) <*> (Second b) = Second (f b)

instance Monad (Sum a) where
  return = pure
  (Second a) >>= f = f a
  (First a) >>= _ = First a

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    oneof [return $ First a, return $ Second b]

instance (Eq a, Eq b) => EqProp (Sum a b) where
  (=-=) = eq

test = do
  let t = undefined :: Sum Int (String, String, String)
  quickBatch $ functor t
  quickBatch $ applicative t
  quickBatch $ monad t