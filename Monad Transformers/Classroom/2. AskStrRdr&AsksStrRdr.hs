{- 
Задание 2: 
Напишите функции askStrRdr и asksStrRdr обеспечивающую трансформер StrRdrT стандартным интерфейсом обращения к окружению:

askStrRdr :: Monad m => StrRdrT m String
askStrRdr = undefined

asksStrRdr :: Monad m => (String -> a) -> StrRdrT m a
asksStrRdr = undefined
Введите для удобства упаковку для StrRdrT Identity и напишите функцию запускающую вычисления в этой монаде

type StrRdr = StrRdrT Identity

runStrRdr :: StrRdr a -> String -> a
runStrRdr = undefined
Тест

srtTst' :: StrRdr (String,Int)
srtTst'  = do
  env <- askStrRdr
  len <- asksStrRdr length
  return (env,len)
должен дать такой результат

GHCi> runStrRdr srtTst' "ABCD"
("ABCD",4)
А тест

stSrTst :: StateT Int StrRdr Int
stSrTst = do
  a <- get
  n <- lift $ asksStrRdr length
  modify (+n)
  return a
такой

GHCi> runStrRdr (runStateT stSrTst 33) "ABCD"
(33,37)
-}

{-# LANGUAGE InstanceSigs #-}

import Control.Monad.Identity (Identity (..))
import Control.Monad.State -- не убирайте, используется при тестировании

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

--------------------------------------
askStrRdr :: (Monad m) => StrRdrT m String
askStrRdr = StrRdrT $ \s -> pure s

asksStrRdr :: (Monad m) => (String -> a) -> StrRdrT m a
asksStrRdr f = StrRdrT $ \s -> pure (f s)

type StrRdr = StrRdrT Identity

runStrRdr :: StrRdr a -> String -> a
runStrRdr m = runIdentity . runStrRdrT m
