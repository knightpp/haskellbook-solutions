module FixerUpper where

task1 = const <$> Just "Hello" <*> pure "World"

task2 = (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> pure [1 .. 3]