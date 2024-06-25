{- 
Задание 2: 
Сделайте тип

newtype Cmps f g x = Cmps { getCmps :: f (g x) }   deriving (Eq,Show) 
представителем класса типов Foldable.

GHCi> maximum $ Cmps [Nothing, Just 2, Just 3]
3
GHCi> length $ Cmps [[1,2], [], [3,4,5,6,7]]
7
-}

{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleContexts #-}

newtype Cmps f g x = Cmps { getCmps :: f (g x) } deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Cmps f g) where
    fmap :: (a -> b) -> Cmps f g a -> Cmps f g b
    fmap h (Cmps x) = Cmps (fmap (fmap h) x)

instance (Applicative f, Applicative g) => Applicative (Cmps f g) where
    pure x = Cmps (pure (pure x))

    (<*>) (Cmps h) (Cmps x) = Cmps (fmap (<*>) h <*> x)

instance (Foldable f, Foldable g) => Foldable (Cmps f g) where
    foldMap h (Cmps x) = foldMap (foldMap h) x

instance (Traversable f, Traversable g) => Traversable (Cmps f g) where
    traverse h (Cmps x) = Cmps <$> traverse (traverse h) x
