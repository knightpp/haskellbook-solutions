module ChapterExercises where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

--- 1
pure1 :: a -> [a]
pure1 = undefined :: a -> [] a

t1 :: [a -> b] -> [a] -> [b]
t1 = undefined :: [] (a -> b) -> [] a -> [] b

--- 2
pure2 :: a -> IO a
pure2 = undefined :: a -> IO a

t2 :: IO (a -> b) -> IO a -> IO b
t2 = undefined :: IO (a -> b) -> IO a -> IO b

--- 3
pure3 :: a -> (a, a)
pure3 = undefined :: a -> (,) a a

t3 :: (a, a -> b) -> (a, a) -> (a, b)
t3 = undefined :: (,) a (a -> b) -> (,) a a -> (,) a b

--- 4
pure4 :: a -> e -> a
pure4 = undefined :: a -> (->) e a

t4 :: (e -> a -> b) -> (e -> a) -> e -> b
t4 = undefined :: (->) e (a -> b) -> (->) e a -> (->) e b

------- PART 2 -------
--- 1
data Pair a = Pair a a deriving (Show, Eq)

instance Functor Pair where
  fmap f (Pair a1 a2) = Pair (f a1) (f a2)

instance Applicative Pair where
  pure a = Pair a a
  (Pair f f') <*> (Pair a a') = Pair (f a) (f' a')

instance (Arbitrary a) => Arbitrary (Pair a) where
  arbitrary = do
    a1 <- arbitrary
    a2 <- arbitrary
    return $ Pair a1 a2

instance Eq a => EqProp (Pair a) where
  (=-=) = eq

test1 :: IO ()
test1 = quickBatch $ applicative (undefined :: Pair (Int, String, Integer))

--- 2
data Two a b = Two a b deriving (Show, Eq)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance (Monoid a) => Applicative (Two a) where
  pure = Two mempty
  (Two a' f') <*> (Two a b) = Two (a <> a') (f' b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b

instance (Eq a, Eq b) => EqProp (Two a b) where
  (=-=) = eq

test2 = quickBatch $ applicative (undefined :: Two String (String, String, String))

--- 3
data Three a b c = Three a b c deriving (Show, Eq)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure = Three mempty mempty
  (Three a b f) <*> (Three a' b' c) = Three (a <> a') (b <> b') (f c)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three a b c

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

test3 = quickBatch $ applicative (undefined :: Three String String (String, String, String))

--- 4
data Three' a b = Three' a b b deriving (Show, Eq)

instance Functor (Three' a) where
  fmap f (Three' a b c) = Three' a (f b) (f c)

instance (Monoid a) => Applicative (Three' a) where
  pure x = Three' mempty x x
  (Three' a f1 f2) <*> (Three' a' b c) = Three' (a <> a') (f1 b) (f2 c)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    a <- arbitrary
    b1 <- arbitrary
    b2 <- arbitrary
    return $ Three' a b1 b2

instance (Eq a, Eq b) => EqProp (Three' a b) where
  (=-=) = eq

test4 = quickBatch $ applicative (undefined :: Three' String (String, String, String))

--- 5
data Four a b c d = Four a b c d deriving (Show, Eq)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
  pure x = Four mempty mempty mempty x
  (Four a b c f) <*> (Four a' b' c' d) = Four (a <> a') (b <> b') (c <> c') (f d)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return $ Four a b c d

instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where
  (=-=) = eq

test5 = quickBatch $ applicative (undefined :: Four String String String (String, String, String))

--- 6
data Four' a b = Four' a a a b deriving (Show, Eq)

instance Functor (Four' a) where
  fmap f (Four' a a' a'' b) = Four' a a' a'' (f b)

instance (Monoid a) => Applicative (Four' a) where
  pure = Four' mempty mempty mempty
  (Four' a1 a2 a3 f) <*> (Four' a1' a2' a3' b) = Four' (a1 <> a1') (a2 <> a2') (a3 <> a3') (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do
    a1 <- arbitrary
    a2 <- arbitrary
    a3 <- arbitrary
    b <- arbitrary
    return $ Four' a1 a2 a3 b

instance (Eq a, Eq b) => EqProp (Four' a b) where
  (=-=) = eq

test6 = quickBatch $ applicative (undefined :: Four' String (String, String, String))

test = do
  test1
  test2
  test3
  test4
  test5
  test6