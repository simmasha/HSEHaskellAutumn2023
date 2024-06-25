{- 
Задание 7: 
Вычислите усреднённый по 
𝑘
k сериям модуль отклонения количества орлов от своего теоретического среднего значения в серии. Предполагается, что монетка "честная", то есть теоретическое среднее известно и равно половине длины серии 
𝑛
n.

Генератор случайных чисел создайте с помощью mkStdGen, инициализировав его каким-то конкретным числом. Решите задачу, не используя монад: явно передавая генератор между вычислениями или используя как источник случайности бесконечный список, возвращаемый randomRs.

avgdev'' :: Int -> Int -> Double
avgdev'' k n = undefined
GHCi> avgdev'' 42 1
0.5
-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
import Control.Monad.Writer
import Control.Monad.State
import Data.IORef
import qualified Control.Monad
import System.Random

res :: [[Int]] -> Int -> Int -> Double
res xs k n = foldr (\ x -> (+) (abs $ fromIntegral (sum x) - fromIntegral n / 2)) 0.0 xs

sublist :: Int -> Int -> [a] -> [a]
sublist fr to s = take  (to - max fr 0) $ drop fr s
movingLists :: Int -> [a] -> [[a]]
movingLists _ [] = []
movingLists k (x:xs) | length(take k (x:xs))>=k = sublist 0 k (x:xs) : movingLists k xs
                     | otherwise = []
avgdev'' :: Int -> Int -> Double
avgdev'' k n = res (movingLists' k n 0) k n / fromIntegral k
movingLists' 0 x y = []
movingLists' a b c =  take b (drop c (randomRs (0, 1) (mkStdGen 42))): movingLists' (a - 1) b (c + b)
movingLists' _ _ _ = undefined
