{- 
Задание 2: 
Сделайте тип данных

newtype Cmps f g x = Cmps { getCmps :: f (g x) }
представителем класса типов Functor:

instance (Functor f, Functor g) => Functor (Cmps f g)
  where fmap = undefined
Проверьте работоспособность на примерах:

GHCi> ffmap h = getCmps . fmap h . Cmps
GHCi> ffmap (+42) $ Just [1,2,3]
Just [43,44,45]
GHCi> ffmap (+42) [Just 1,Just 2,Nothing]
[Just 43,Just 44,Nothing]
-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall #-}

newtype Cmps f g x = Cmps { getCmps :: f (g x) }

instance (Functor f, Functor g) => Functor (Cmps f g) where
  fmap :: (Functor f, Functor g) => (a -> b) -> Cmps f g a -> Cmps f g b
  fmap f (Cmps c) = Cmps $ (f <$>) <$> c
