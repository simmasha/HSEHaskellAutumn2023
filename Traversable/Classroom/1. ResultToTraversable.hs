{- 
Задание 1: 
Сделайте тип данных

data Result a = Ok a | Error String 
  deriving (Eq,Show)
представителем класса типов Traversable:

GHCi> traverse (\x->[x+2,x-2]) (Ok 5)
[Ok 7,Ok 3]
GHCi> traverse (\x->[x+2,x-2]) (Error "!!!")
[Error "!!!"]
-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wall #-}

import Data.Traversable (fmapDefault, foldMapDefault)

data Result a
  = Ok a
  | Error String
  deriving (Eq, Show)

instance Functor Result where
  fmap :: (a -> b) -> Result a -> Result b
  fmap = fmapDefault

instance Foldable Result where
  foldMap :: (Monoid m) => (a -> m) -> Result a -> m
  foldMap = foldMapDefault

instance Traversable Result where
  traverse :: (Applicative f) => (a -> f b) -> Result a -> f (Result b)
  traverse _ (Error e) = pure $ Error e
  traverse f (Ok a) = Ok <$> f a
