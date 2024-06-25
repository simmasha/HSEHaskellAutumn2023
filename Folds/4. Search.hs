{- 
Задание 4: 
Напишите реализацию оператора "безопасного" поиска элемента списка по индексу (!!!) :: [a] -> Int -> Maybe a через foldr:

GHCi> "abcdefgh" !!! 5
Just 'f'
GHCi> "abcdefgh" !!! (-1)
Nothing
GHCi> "abcdefgh" !!! 100
Nothing
Используйте технику дополнительного параметра:

(!!!) :: [a] -> Int -> Maybe a
xs !!! n = foldr fun ini xs n 
fun = undefined
ini = undefined
-}

infixl 9 !!!

(!!!) :: [a] -> Int -> Maybe a
xs !!! n = foldr fun ini xs n

fun :: a -> (Int -> Maybe a) -> Int -> Maybe a
fun c g n
    | n > 0 = g (n - 1)
    | n == 0 = Just c
    | otherwise = Nothing

ini :: Int -> Maybe a
ini _ = Nothing
