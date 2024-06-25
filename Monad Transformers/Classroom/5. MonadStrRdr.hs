{- 
Задание 5: 
Чтобы избавится от необходимости ручного подъема операций askStrRdr и asksStrRdr, обеспечивающих стандартный интерфейс вложенного трансформера StrRdrT, можно поступить по аналогии с другими трансформерами библиотеки mtl. А именно, разработать класс типов MonadStrRdr, выставляющий этот стандартный интерфейс для нашего ридера:

class Monad m => MonadStrRdr m where
  askSR :: m String
  asksSR :: (String -> a) -> m a
  strRdr :: (String -> a) -> m a
(Замечание. Мы переименовываем функцию askStrRdr в askSR (и анологично asks-версию), поскольку хотим держать всю реализацию в одном файле исходного кода. При следовании принятой в библиотеках transformers/mtl идеологии они имели бы одно и то же имя, но были бы определены в разных модулях. При работе с transformers мы импортировали бы свободную функцию c квалифицированным именем Control.Monad.Trans.StrRdr.askStrRdr, а при использовании mtl работали бы с методом Control.Monad.StrRdr.askStrRdr класса типов MonadStrRdr.)

Этот интерфейс, во-первых, должен выставлять сам трансформер StrRdrT, обернутый вокруг произвольной монады:

instance Monad m => MonadStrRdr (StrRdrT m) where
  askSR :: StrRdrT m String
  askSR = undefined
  asksSR :: (String -> a) -> StrRdrT m a
  asksSR = undefined
  strRdr :: (String -> a) -> StrRdrT m a
  strRdr = undefined
Реализуйте этого представителя, для проверки используйте

srStTst'' :: StrRdrT (State Int) (Int,Int,Int,String)
srStTst'' = do 
  m <- get
  n <- asksStrRdr length -- use asksStrRdr
  k <- strRdr length     -- use strRdr
  put $ m + n + k
  e <- askStrRdr         -- use askStrRdr
  return (m,n,k,e)
Результат должен быть таким:

GHCi> runState (runStrRdrT srStTst'' "ABCDE") 2  
((2,5,5,"ABCDE"),12)
Во-вторых, интерфейс MonadStrRdr должен выставлять любой стандартный трансформер, обернутый вокруг произвольной монады, выставляющей этот интерфейс:

instance MonadStrRdr m => MonadStrRdr (StateT s m) where
  askSR :: StateT s m String
  askSR =  undefined
  asksSR :: (String -> a) -> StateT s m a
  asksSR = undefined
  strRdr :: (String -> a) -> StateT s m a
  strRdr = undefined

-- WriterT w, etc...
Реализуйте этого представителя для стандартного StateT, для проверки используйте

stSrTst' :: StateT Int StrRdr (Int,Int,Int,String)
stSrTst' = do
  a <- get
  n <- asksSR length   -- no lift!
  k <- strRdr length   -- no lift!
  e <- askSR           -- no lift!
  modify (+n)
  return (a,n,k,e)
Результат должен быть таким:

GHCi> runStrRdr (runStateT stSrTst' 33) "ABCD"
((33,4,4,"ABCD"),37)
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
  get = lift get
  put :: (MonadState s m) => s -> StrRdrT m ()
  put = lift . put
  state :: (MonadState s m) => (s -> (a, s)) -> StrRdrT m a
  state = lift . state

--------------------------------------
class (Monad m) => MonadStrRdr m where
  askSR :: m String
  asksSR :: (String -> a) -> m a
  strRdr :: (String -> a) -> m a

instance (Monad m) => MonadStrRdr (StrRdrT m) where
  askSR :: StrRdrT m String
  askSR = askStrRdr
  asksSR :: (String -> a) -> StrRdrT m a
  asksSR = asksStrRdr
  strRdr :: (String -> a) -> StrRdrT m a
  strRdr = asksStrRdr

instance (MonadStrRdr m) => MonadStrRdr (StateT s m) where
  askSR :: StateT s m String
  askSR = lift askSR
  asksSR :: (String -> a) -> StateT s m a
  asksSR = lift . asksSR
  strRdr :: (String -> a) -> StateT s m a
  strRdr = lift . strRdr
