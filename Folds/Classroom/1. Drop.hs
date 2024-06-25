{- 
Задание 1: 
Напишите реализацию функции drop через foldr:

GHCi> drop' 6 "Hello World!" == "World!"
True
GHCi> drop' 3 [1,2] == []
True
Используйте технику дополнительного параметра:

drop' :: Int -> [a] -> [a]
drop' n xs = foldr step ini xs n
step = undefined
ini = undefined
-}

drop' :: Int -> [a] -> [a]
drop' n xs = foldr step ini xs n

step :: a -> (Int -> [a]) -> Int -> [a]
step e g n
    | n <= 0 = e : g 0
    | otherwise = g (n - 1)

ini :: Int -> [a]
ini _ = []
