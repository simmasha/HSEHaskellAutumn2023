{- 
Задание 3: 
Напишите функцию, вычисляющую числа Фибоначчи с использованием монады State.

fib :: Int -> Integer
fib n = fst $ execState (replicateM n fibStep) (0,1)

fibStep :: State (Integer,Integer) ()
fibStep = do undefined
GHCi> fib 7
13
-}
import Control.Monad.State
fib :: Int -> Integer
fib n = fst $ execState (replicateM n fibStep) (0,1)

fibStep :: State (Integer,Integer) ()
fibStep = do 
     pair <- get
     let y = fst pair
     let x = snd pair
     put (x+y, y)
