{- 
Задание 2: 
Сделайте тип данных непустого списка

data NEList a = Single a | Cons a (NEList a)
  deriving (Eq,Show)
представителем класса типов Traversable. Выполните реализацию представителя через функцию sequenceA.

GHCi> sequenceA $ Cons (Right 3) (Cons (Right 4) (Single (Right 5)))
Right (Cons 3 (Cons 4 (Single 5)))
GHCi> sequenceA $ Cons (Right 3) (Cons (Left 4) (Single (Right 5)))
Left 4
GHCi> traverse (\x->[x+2,x-2]) (Cons 20 (Single 30))
[Cons 22 (Single 32),Cons 22 (Single 28),Cons 18 (Single 32),Cons 18 (Single 28)]
-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wall #-}

import Data.Traversable (foldMapDefault)

data NEList a
  = Single a
  | Cons a (NEList a)
  deriving (Eq, Show)

instance Functor NEList where
  -- can't use `fmapDefault` from `Data.Traversable`
  -- on Traversable instances defined with only sequenceA
  -- bc it'll result in infinite recursion
  fmap :: (a -> b) -> NEList a -> NEList b
  fmap f (Single v) = Single $ f v
  fmap f (Cons a rest) = Cons (f a) $ f <$> rest

instance Foldable NEList where
  foldMap :: (Monoid m) => (a -> m) -> NEList a -> m
  foldMap = foldMapDefault

instance Traversable NEList where
  sequenceA :: (Applicative f) => NEList (f a) -> f (NEList a)
  sequenceA (Single v) = Single <$> v
  sequenceA (Cons a rest) = Cons <$> a <*> sequenceA rest
