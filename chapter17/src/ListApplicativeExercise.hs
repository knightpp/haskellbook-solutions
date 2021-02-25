module ListApplicativeExercise where

import Test.QuickCheck 
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data List a
  = Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap f x = case x of
    Nil -> Nil
    Cons a rest -> Cons (f a) (fmap f rest)

instance (Arbitrary a) => Arbitrary (List a) where
  arbitrary = do
    a <- arbitrary
    a1 <- arbitrary
    a2 <- arbitrary

    frequency
      [ (1, return $ Cons a Nil),
        (1, return $ Cons a (Cons a1 Nil)),
        (1, return (Cons a (Cons a1 (Cons a2 Nil)))),
        (1, return Nil)
      ]

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

instance Eq a => EqProp (List a) where
    (=-=) = eq

instance Applicative List where
  pure a = Cons a Nil
  f <*> x = case f of
    Nil -> Nil
    Cons f fs -> fmap f x `append` (fs <*> x)

test = quickBatch $ applicative (undefined :: List (Int, Int, Int))
