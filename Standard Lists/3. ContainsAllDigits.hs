{- 
Задание 3: 
Определите, содержит ли заданное целое число все цифры от 1 до 9. (Используйте функцию digits из предыдущего задания.)

containsAllDigits :: Integer -> Bool
containsAllDigits = undefined
GHCi> containsAllDigits (-123455556789)
True
-}
digits :: Integer -> [Integer]
digits n
    | n<0 = digits (-n)
    | n<10 = [n]
    | otherwise = digits (n `div` 10) ++ [n `mod` 10]

containsAllDigits :: Integer -> Bool
containsAllDigits n = all (`elem` digitList) [1..9]
    where digitList = digits n
