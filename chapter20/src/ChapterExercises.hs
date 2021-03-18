module ChapterExercises where

-- | task 1
data Constant a b
  = Constant a

instance Foldable (Constant a) where
  foldMap _ _ = mempty

-- | task 2
data Two a b
  = Two a b

instance Foldable (Two a) where
  foldMap f (Two a b) = f b
  foldr f start (Two a b) = f b start

-- | task 3
data Three a b c
  = Three a b c

instance Foldable (Three a b) where
  foldMap f (Three _ _ c) = f c

-- | task 4
data Three' a b
  = Three' a b b

instance Foldable (Three' a) where
  foldMap f (Three' a b b') = f b <> f b'

-- | task 5
data Four' a b
  = Four' a b b b

instance Foldable (Four' a) where
  foldMap f (Four' a b b' b'') = f b <> f b' <> f b''

-- | Thinking cap time
filterF ::
  (Applicative f, Foldable t, Monoid (f a)) =>
  (a -> Bool) ->
  t a ->
  f a
filterF f =
  foldr
    (\a acc -> if f a then pure a <> acc else acc)
    mempty
