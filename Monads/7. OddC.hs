{- 
Задание 7: 
Сделайте тип данных

data OddC a = Un a | Bi a a (OddC a) deriving (Eq,Show)
представителем классов типов Functor, Applicative и Monad. Семантика должна быть подобной семантике представителей этих классов типов для списков: монада OddC должна иметь эффект вычисления с произвольным нечетным числом результатов:

GHCi> tst1 = Bi 10 20 (Un 30)
GHCi> tst2 = Bi 1 2 (Bi 3 4 (Un 5))
GHCi> do {x <- tst1; y <- tst2; return (x + y)}
Bi 11 12 (Bi 13 14 (Bi 15 21 (Bi 22 23 (Bi 24 25 (Bi 31 32 (Bi 33 34 (Un 35)))))))
GHCi> do {x <- tst2; y <- tst1; return (x + y)}
Bi 11 21 (Bi 31 12 (Bi 22 32 (Bi 13 23 (Bi 33 14 (Bi 24 34 (Bi 15 25 (Un 35)))))))
-}

import Control.Monad
concat3OC :: OddC a -> OddC a -> OddC a -> OddC a
concat3OC (Un a) (Un b) w = Bi a b w
concat3OC (Bi a b c) y z = Bi a b (concat3OC c y z) 
concat3OC (Un a) (Bi x y z) w = Bi a x (concat3OC (Un y) z w)
instance Functor OddC where
  fmap f (Un a) = Un (f a)
  fmap f (Bi a b c) = Bi (f a) (f b) (fmap f c)
instance Applicative OddC where
   pure  = return 
   (<*>) = ap
instance Monad OddC where
    (Bi a b c) >>= k = concat3OC (k a) (k b) (c >>= k)
    (Un a) >>= k = k a
    return = Un
