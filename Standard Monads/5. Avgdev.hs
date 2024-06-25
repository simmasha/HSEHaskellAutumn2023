{- 
Задание 5: 
Вычислите усреднённый по 
𝑘
k сериям модуль отклонения количества орлов от своего теоретического среднего значения в серии. Предполагается, что монетка "честная", то есть теоретическое среднее известно и равно половине длины серии 
𝑛
n.

Используйте глобальный системный генератор случайных чисел.

avgdev :: Int -> Int -> IO Double
avgdev k n = undefined
GHCi> avgdev 42 1
0.5
-}

import Control.Monad.Writer
import Control.Monad.State
import Data.IORef
import qualified Control.Monad
import System.Random
avgdev :: Int -> Int -> IO Double
avgdev k n = do
    rand_list <- replicateM k ((replicateM n $ randomRIO (0,1)) :: IO [Int])
    return (res rand_list k n / fromIntegral k)
    
res xs k n = foldr (\ x -> (+) (abs $ fromIntegral (sum x) - fromIntegral n / 2)) 0.0 xs
