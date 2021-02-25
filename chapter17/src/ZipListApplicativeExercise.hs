module ZipListApplicativeExercise where

import ListApplicativeExercise
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

take' :: Int -> List a -> List a
take' n l = reverse' $ go n l Nil
  where
    go _ Nil outList = outList
    go n l@(Cons a as) outList =
      if n == 0
        then outList
        else go (n - 1) as (Cons a outList)

reverse' :: List a -> List a
reverse' l = go l Nil
  where
    go Nil out = out
    go (Cons a as) out = go as (Cons a out)

newtype ZipList' a = ZipList' (List a)
  deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where
      xs' =
        let (ZipList' l) = xs
         in take' 3000 l
      ys' =
        let (ZipList' l) = ys
         in take' 3000 l

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

toList :: Foldable t => t a -> List a
toList l = foldr Cons Nil l
 
toZL :: [a] -> ZipList' a
toZL l = ZipList' $ toList l 

instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    oneof
      [ return (toZL [a, b, c]),
        return (toZL [a, b]),
        return (toZL [a]),
        return $ toZL []
      ]

instance Applicative ZipList' where
  pure x = ZipList' (Cons x Nil)
  ZipList' a <*> ZipList' b = ZipList' (reverse' $ go a b Nil)
    where
      go Nil _ out = out
      go _ Nil _ = Nil
      go (Cons f fs) (Cons b bs) out = go fs bs (Cons (f b) out)
      -- go (Cons a as) (Cons b bs) out = go as bs (Cons (a b) out)

validate = quickBatch (applicative (undefined :: ZipList' (Int, Int, Int)))

tests = do
  let toList x = foldr Cons Nil x
  let list = foldr Cons Nil [1, 2, 3, 4, 5]
  print $ take' 0 list == Nil
  print $ take' 1 list == toList [1]
  print $ take' 2 list == toList [1, 2]
  print $ take' 3 list == toList [1, 2, 3]
  print $ take' 4 list == toList [1, 2, 3, 4]
  print $ take' 5 list == toList [1, 2, 3, 4, 5]