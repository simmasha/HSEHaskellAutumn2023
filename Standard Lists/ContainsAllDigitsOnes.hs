{- 
Задание : 
Определите, содержит ли заданное целое число все цифры от 1 до 9 в точности по одному разу. (Используйте функцию digits из предыдущего задания.)

containsAllDigitsOnes :: Integer -> Bool
containsAllDigitsOnes = undefined
GHCi> containsAllDigitsOnes(-120003400567809)
True
-}
import Data.List

digits :: Integer -> [Integer]
digits n
    | n<0 = digits (-n)
    | n<10 = [n]
    | otherwise = digits (n `div` 10) ++ [n `mod` 10]
    

containsAllDigitsOnes :: Integer -> Bool
containsAllDigitsOnes n = (sort $ filter (/=0) (digits n)) == [1, 2, 3, 4, 5, 6, 7, 8, 9]
