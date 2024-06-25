{- 
Задание 2: 
Напишите функцию write2log обеспечивающую трансформер LoggT стандартным логгирующим интерфейсом:

write2log :: Monad m => String -> LoggT m ()
write2log = undefined
Эта функция позволяет пользователю осуществлять запись в лог в процессе вычисления в монаде LoggT m для любой монады m. Введите для удобства упаковку для LoggT Identity и напишите функцию запускающую вычисления в этой монаде

type Logg = LoggT Identity

runLogg :: Logg a -> Logged a
runLogg = undefined
Тест

logTst' :: Logg Integer   
logTst' = do 
  write2log "AAA"
  write2log "BBB"
  return 42
должен дать такой результат:

GHCi> runLogg logTst'
Logged "BBBAAA" 42
А тест

stLog :: StateT Integer Logg Integer
stLog = do 
  modify (+1)
  a <- get
  lift $ write2log $ show $ a * 10
  put 42
  return $ a * 100
такой

GHCi> runLogg $ runStateT stLog 2
Logged "30" (300,42)
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

write2log :: Monad m => String -> LoggT m ()
write2log s = LoggT $ return $ Logged s () 

type Logg = LoggT Identity
    
runLogg :: Logg a -> Logged a
runLogg = runIdentity . runLoggT
