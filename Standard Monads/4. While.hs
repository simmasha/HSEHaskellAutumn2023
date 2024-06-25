{- 
Задание 4: 
Напишите функцию

while :: IORef a -> (a -> Bool) -> IO () -> IO ()
while ref p action = do undefined
позволяющую кодировать "императивные циклы" следующего вида:

factorial :: Integer -> IO Integer
factorial n = do
  r <- newIORef 1
  i <- newIORef 1
  while i (<= n) ( do
    ival <- readIORef i
    modifyIORef' r (* ival)
    modifyIORef' i (+ 1)
   )
  readIORef r
-}
-- Функцию factorial реализовывать не надо!
import Data.IORef
import qualified Control.Monad
while :: IORef a -> (a -> Bool) -> IO () -> IO ()
while ref p action = do
    i <- readIORef ref
    Control.Monad.when (p i) $ do
       action
       while ref p action
