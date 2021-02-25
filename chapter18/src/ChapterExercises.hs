module ChapterExercises where
import Test.QuickCheck (Arbitrary (arbitrary), oneof)
import Test.QuickCheck.Checkers (EqProp (..), eq, quickBatch)
import Test.QuickCheck.Classes (applicative, functor, monad, monoid, semigroup)

--- task 1
data Nope a = NopeDotJpg deriving (Show, Eq)

instance Functor Nope where
  fmap _ _ = NopeDotJpg

instance Applicative Nope where
  pure _ = NopeDotJpg
  _ <*> _ = NopeDotJpg

instance Monad Nope where
  _ >>= _ = NopeDotJpg

instance Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

instance EqProp (Nope a) where
  (=-=) = eq

test1 = do
  let t = undefined :: Nope (String, String, String)
  quickBatch $ functor t
  quickBatch $ applicative t
  quickBatch $ monad t

--- task 2

data PhhhbbtttEither b a
  = Left' a
  | Right' b
  deriving (Show, Eq)

instance Functor (PhhhbbtttEither b) where
  fmap f x = case x of
    Left' a -> Left' $ f a
    Right' b -> Right' b

instance Applicative (PhhhbbtttEither b) where
  pure = Left'
  Left' f <*> Left' x = Left' $ f x
  _ <*> Right' a = Right' a
  Right' a <*> _ = Right' a

instance Monad (PhhhbbtttEither b) where
  (>>=) = undefined

-- TODO:

--- task 3
newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance Applicative Identity where
  pure = Identity
  (Identity f) <*> (Identity x) = Identity $ f x

instance Monad Identity where
  return = pure
  Identity a >>= f = f a

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

test3 = do
  let t = undefined :: Identity (String, String, String)
  quickBatch $ functor t
  quickBatch $ applicative t
  quickBatch $ monad t

--- task 4

data List a = Nil | Cons a (List a) deriving (Show, Eq)

instance Functor List where
  fmap f x = case x of
    Nil -> Nil
    Cons a as -> Cons (f a) (fmap f as)

instance Applicative List where
  pure = flip Cons Nil
  Nil <*> _ = Nil
  Cons f fs <*> xs = fmap f xs <> (fs <*> xs)

instance Semigroup (List a) where
  Nil <> bs = bs
  (Cons a as) <> bs = Cons a (as <> bs)

instance Monoid (List a) where
  mempty = Nil

instance Monad List where
  Nil >>= _ = Nil
  Cons x xs >>= f = f x <> (xs >>= f)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    oneof
      [ return (Cons a Nil),
        return (Cons a (Cons b Nil)),
        return (Cons a (Cons b (Cons c Nil)))
      ]

instance Eq a => EqProp (List a) where
  (=-=) = eq


test4 = do
  let t = undefined :: List (String, String, String)
  quickBatch $ monoid t
  quickBatch $ functor t
  quickBatch $ applicative t
  quickBatch $ monad t

