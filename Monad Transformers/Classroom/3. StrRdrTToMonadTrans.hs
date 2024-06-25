{- 
Задание 3: 
В последнем примере функция

lift :: (MonadTrans t, Monad m) => m a -> t m a
позволяла поднять вычисление из внутренней монады (в примере это был StrRdr) во внешний трансформер (StateT Int). Это возможно, поскольку для трансформера StateT s реализован представитель класса типов MonadTrans. Сделайте трансформер StrRdrT представителем класса MonadTrans, так чтобы можно было поднимать вычисления из произвольной внутренней монады в наш трансформер:

instance MonadTrans StrRdrT where
  lift = undefined
Сценарий использования:

srStTst :: StrRdrT (State Int) (Int,Int)
srStTst = do
  lift $ state $ \s -> ((),s + 40)
  m <- lift get
  n <- asksStrRdr length
  lift $ put $ m + n
  return (m,n)
Проверка:

GHCi> runState (runStrRdrT srStTst "ABCD") 2
((42,4),46)
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

askStrRdr :: (Monad m) => StrRdrT m String
askStrRdr = StrRdrT $ \s -> pure s

asksStrRdr :: (Monad m) => (String -> a) -> StrRdrT m a
asksStrRdr f = StrRdrT $ \s -> pure (f s)

type StrRdr = StrRdrT Identity

runStrRdr :: StrRdr a -> String -> a
runStrRdr m = runIdentity . runStrRdrT m

--------------------------------------
instance MonadTrans StrRdrT where
  lift :: Monad m => m a -> StrRdrT m a
  lift = StrRdrT . const
