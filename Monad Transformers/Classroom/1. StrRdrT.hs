{- 
Задание 1: 
Сделайте на основе однопараметрического типа данных String -> a (фактически это Reader с окружением строкового типа) трансформер монад StrRdrT :: (* -> *) -> * -> * с одноименным конструктором данных и меткой поля runStrRdrT:

newtype StrRdrT m a = StrRdrT { runStrRdrT :: String -> m a }
GHCi> :t StrRdrT
StrRdrT :: (String -> m a) -> StrRdrT m a
GHCi> :t runStrRdrT
runStrRdrT :: StrRdrT m a -> String -> m a
Для этого реализуйте для произвольной монады m представителя класса типов Monad для StrRdrT m :: * -> *:

instance Monad m => Monad (StrRdrT m) where
  return :: a -> StrRdrT m a
  return = undefined

  (>>=) :: StrRdrT m a -> (a -> StrRdrT m b) -> StrRdrT m b
  (>>=) = undefined
Поскольку StrRdr не подразумевает специфического умения обрабатывать ошибки, семантика MonadFail должна протаскиваться из внутренней монады. Реализуйте соответствующего представителя

instance MonadFail m => MonadFail (StrRdrT m)  where
  fail :: String -> StrRdrT m a
  fail  = undefined
Для проверки используйте функции:

srtTst :: StrRdrT Identity Int
srtTst = do
  x <- StrRdrT $ Identity <$> length
  y <- return 10
  return $ x + y

failTst :: StrRdrT [] Integer
failTst = do
  'z' <- StrRdrT id
  return 42
которые при правильной реализации монады должны вести себя так:

GHCi> runStrRdrT srtTst "ABCDE"
Identity 15
GHCi> runIdentity (runStrRdrT srtTst "ABCDE")
15
GHCi> runStrRdrT failTst "zanzibar"
[42,42]
GHCi> runStrRdrT failTst "zzz..."
[42,42,42]
GHCi> runStrRdrT failTst "ABCD"
[]
-}

{-# LANGUAGE InstanceSigs #-}

import Control.Monad.Identity (Identity (..), ap)

newtype StrRdrT m a = StrRdrT {runStrRdrT :: String -> m a}

instance (Monad m) => Monad (StrRdrT m) where
  return :: a -> StrRdrT m a
  return = StrRdrT . const . pure

  (>>=) :: StrRdrT m a -> (a -> StrRdrT m b) -> StrRdrT m b
  m >>= k = StrRdrT $ \s ->
    let r = runStrRdrT m s
     in r >>= (\a -> runStrRdrT (k a) s)

instance (MonadFail m) => MonadFail (StrRdrT m) where
  fail :: String -> StrRdrT m a
  fail e = StrRdrT $ \_ -> fail e

instance (Monad m) => Functor (StrRdrT m) where
  fmap f (StrRdrT origF) = StrRdrT (fmap f . origF)

instance (Monad m) => Applicative (StrRdrT m) where
  pure = return
  (<*>) = ap
