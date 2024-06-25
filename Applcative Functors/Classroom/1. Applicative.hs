{- 
Задание 1: 
Для типа данных, изоморфного Either,

data E l r = L l | R r
    deriving (Eq, Show)
напишите представителя класса типов Applicative с семантикой, подобной Maybe:

GHCi> max <$> R 5 <*> R 3
R 5
GHCi> min <$> R 2 <*> L "forty two"
L "forty two"
Не забудьте для E реализовать представителя класса типов Functor.
-}

{-# LANGUAGE InstanceSigs #-}

data E l r = L l | R r
    deriving (Eq, Show)

instance Functor (E l) where
  fmap :: (a -> b) -> E l a -> E l b
  fmap _ (L l) = L l
  fmap f (R a) = R $ f a

instance Applicative (E l) where
  pure :: a -> E l a
  pure  = R

  (<*>) :: E l (a -> b) -> E l a -> E l b
  L l <*> _ = L l
  _ <*> L l = L l
  R ab <*> R a = R $ ab a
