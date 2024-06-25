{- 
Задание 4: 
Сделайте тип

newtype Cmps f g x = Cmps { getCmps :: f (g x) }   deriving (Eq,Show) 
представителем класса типов Applicative.

GHCi> getCmps $ (+) <$> Cmps [Just 1,Just 2] <*> Cmps [Nothing,Just 40]
[Nothing,Just 41,Nothing,Just 42]
-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleContexts #-}

newtype Cmps f g x = Cmps { getCmps :: f (g x) }

instance (Functor f, Functor g) => Functor (Cmps f g) where
  fmap :: (a -> b) -> Cmps f g a -> Cmps f g b
  fmap h (Cmps x) = Cmps (fmap (fmap h) x)

instance (Applicative f, Applicative g) => Applicative (Cmps f g) where
    pure x = Cmps (pure (pure x))

    (<*>) (Cmps h) (Cmps x) = Cmps (fmap (<*>) h <*> x)
