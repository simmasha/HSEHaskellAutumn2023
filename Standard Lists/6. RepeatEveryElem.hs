{- 
Задание 6: 
Повторите каждый элемент списка заданное число раз.

repeatEveryElem :: Int -> [a] -> [a]
repeatEveryElem = undefined
GHCi> repeatEveryElem 3 "abcd"
"aaabbbcccddd"
-}
repeatEveryElem :: Int -> [a] -> [a]
repeatEveryElem n = concatMap (replicate n)
