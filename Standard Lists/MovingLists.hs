{- 
Задание 7: 
Дан список (возможно бесконечный) [a1, a2, …] и положительное целое число n. Создайте спиcок "скользящих" подсписков длины n, то есть список списков следующего вида: 
[a1,…,an],[a2,…,an+1],[a3,…,an+2],…]

movingLists :: Int -> [a] -> [[a]]
movingLists = undefined

GHCi> movingLists 2 [5..8]
[[5,6],[6,7],[7,8]]
-}
import Data.List

movingLists :: Int -> [a] -> [[a]]
movingLists n xs = takeWhile (\l -> length l == n) (map (take n) (tails xs))
