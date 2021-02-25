module ChapterExercisesPart2 where

import Control.Monad
import Test.QuickCheck (Arbitrary (arbitrary), oneof)
import Test.QuickCheck.Checkers (EqProp (..), eq, quickBatch)
import Test.QuickCheck.Classes (applicative, functor, monad, monoid, semigroup)

--- task 1
j :: Monad m => m (m a) -> m a
j = (=<<) id

-- j = join

--- task 2
l1 :: Monad m => (a -> b) -> m a -> m b
-- l1 f a = f <$> a
l1 f a = a >>= (return . f)

--- task 3
l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
-- l2 f a b = do
--     a' <- a
--     b' <- b
--     return (f a' b')
l2 = liftM2

--- task 4
a :: Monad m => m a -> m (a -> b) -> m b
a x f = f <*> x

--- task 5
meh :: Monad m => [a] -> (a -> m b) -> m [b]
-- meh as f = sequence (fmap f as)
-- meh as f = mapM f as
meh as f = collect $ fmap f as
  where
    collect :: Monad m => [m a] -> m [a]
    collect [] = pure []
    collect (ma : mas) = do
      a <- ma
      collect mas

--- task 6
flipType :: (Monad m) => [m a] -> m [a]
flipType ma = meh ma id
