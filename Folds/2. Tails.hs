{- 
Задание 2: 
Напишите реализации функций из стандартной библиотеки tails, inits :: [a] -> [[a]] через свёртку foldr:

tails' :: [a] -> [[a]]
tails' = foldr fun ini
fun = undefined
ini = undefined

inits' :: [a] -> [[a]]
inits' = foldr fun' ini'
fun' = undefined
ini' = undefined
GHCi> inits' [1,2,3]
[[],[1],[1,2],[1,2,3]]
-}

tails' :: [a] -> [[a]]
tails' = foldr fun ini

fun :: a -> [[a]] -> [[a]]
fun c b = (c : head b) : b
ini = [[]]

inits' :: [a] -> [[a]]
inits' = foldr fun' ini'

fun' :: a -> [[a]] -> [[a]]
fun' c b = [] : map (c:) b
ini' = [[]]
