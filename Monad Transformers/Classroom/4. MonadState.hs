{- 
Задание 4: 
Избавьтесь от необходимости ручного подъема операций вложенной монады State, сделав трансформер StrRdrT, примененный к монаде с интерфейсом MonadState представителем этого (MonadState) класса типов:

instance MonadState s m => MonadState s (StrRdrT m) where
  get   = undefined
  put   = undefined
  state = undefined
Сценарий использования:

srStTst' :: StrRdrT (State Int) (Int,Int)
srStTst' = do
  state $ \s -> ((),s + 40)   -- no lift!
  m <- get                    -- no lift!
  n <- asksStrRdr length
  put $ m + n                 -- no lift!
  return (m,n)
Проверка:

GHCi> runState (runStrRdrT srStTst' "ABCDE") 2
((42,5),47)
-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

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

askStrRdr :: (Monad m) => StrRdrT m String
askStrRdr = StrRdrT $ \s -> pure s

asksStrRdr :: (Monad m) => (String -> a) -> StrRdrT m a
asksStrRdr f = StrRdrT $ \s -> pure (f s)

type StrRdr = StrRdrT Identity

runStrRdr :: StrRdr a -> String -> a
runStrRdr m = runIdentity . runStrRdrT m

instance MonadTrans StrRdrT where
  lift :: (Monad m) => m a -> StrRdrT m a
  lift = StrRdrT . const

instance (MonadState s m) => MonadState s (StrRdrT m) where
  get :: (MonadState s m) => StrRdrT m s
  get = StrRdrT $ const get
  put :: (MonadState s m) => s -> StrRdrT m ()
  put s = StrRdrT $ const (put s)
  state :: (MonadState s m) => (s -> (a, s)) -> StrRdrT m a
  state f = StrRdrT $ const (state f)
