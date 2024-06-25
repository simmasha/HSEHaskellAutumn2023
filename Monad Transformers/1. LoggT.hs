{- 
Задание 1: 
делайте на основе типа данных (семантика монады для него обсуждалась на прошлом занятии)

data Logged a = Logged String a deriving (Eq,Show)
трансформер монад LoggT :: (* -> *) -> * -> * с одноименным конструктором данных и меткой поля runLoggT:

newtype LoggT m a = LoggT { runLoggT :: m (Logged a) }
Для этого реализуйте для произвольной монады m представителя класса типов Monad для LoggT m :: * -> *:

instance Monad m => Monad (LoggT m) where
  return x = undefined
  m >>= k  = undefined
а также класса типов MonadFail:

instance MonadFail m => MonadFail (LoggT m) where
  fail msg = undefined
Для проверки используйте функции:

logTst :: LoggT Identity Integer
logTst = do 
  x <- LoggT $ Identity $ Logged "AAA" 30
  y <- return 10
  z <- LoggT $ Identity $ Logged "BBB" 2
  return $ x + y + z
  
failTst :: [Integer] -> LoggT [] Integer
failTst xs = do
  5 <- LoggT $ fmap (Logged "") xs
  LoggT [Logged "A" ()]
  return 42
которые при правильной реализации монады должны вести себя так:

GHCi> runIdentity (runLoggT logTst)
Logged "BBBAAA" 42
GHCi> runLoggT $ failTst [5,5]
[Logged "A" 42,Logged "A" 42]
GHCi> runLoggT $ failTst [5,6]
[Logged "A" 42]
GHCi> runLoggT $ failTst [7,6]
[]
-}

import Control.Monad
import Control.Monad.Identity
data Logged a = Logged String a deriving (Eq,Show)

newtype LoggT m a = LoggT { runLoggT :: m (Logged a) }

instance Monad m => Functor (LoggT m) where
  fmap = liftM

instance Monad m => Applicative (LoggT m) where
  pure = return
  (<*>) = ap
  
instance Monad m => Monad (LoggT m) where
  return  = LoggT . return . Logged mempty 
  m >>= k  = LoggT $ do
      Logged v a <- runLoggT m
      Logged u b <- runLoggT (k a)
      return (Logged (u <> v) b)
instance MonadFail m => MonadFail (LoggT m) where
  fail = LoggT . fail

logTst :: LoggT Identity Integer
logTst = do 
    x <- LoggT $ Identity $ Logged "AAA" 30
    y <- return 10
    z <- LoggT $ Identity $ Logged "BBB" 2
    return $ x + y + z
    
failTst :: [Integer] -> LoggT [] Integer
failTst xs = do
    5 <- LoggT $ fmap (Logged "") xs
    LoggT [Logged "A" ()]
    return 42
