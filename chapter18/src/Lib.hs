module Lib
  ( someFunc,
  )
where

import Control.Monad

someFunc :: IO ()
someFunc = putStrLn "someFunc"

bind :: Monad m => (a -> m b) -> m a -> m b
bind f x = join $ f <$> x