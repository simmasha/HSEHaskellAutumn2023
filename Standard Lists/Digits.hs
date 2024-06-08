{- 
Задание 2: 
Сформируйте список цифр заданного целого числа.

digits :: Integer -> [Integer]
digits = undefined
GHCi> digits (-103)
[1,0,3]
-}
digits :: Integer -> [Integer]
digits n
    | n<0 = digits (-n)
    | n<10 = [n]
    | otherwise = digits (n `div` 10) ++ [n `mod` 10]
