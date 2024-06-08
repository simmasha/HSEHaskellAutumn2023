{- 
Задание 6: 
Реализуйте функцию, находящую сумму и количество цифр заданного целого числа.

sum'n'count :: Integer -> (Integer, Integer)
sum'n'count x = undefined
GHCi> sum'n'count (-39)
(12,2)
-}
sum'n'count :: Integer -> (Integer, Integer)
sum'n'count x
    | x < 0 = sum'n'count $ abs x
    | x < 10 = (x, 1)
    | otherwise = (sum + sum', cnt + cnt')
    where
        (sum', cnt') = sum'n'count $ x `div` 10
        (sum, cnt) = sum'n'count $ x `mod` 10
