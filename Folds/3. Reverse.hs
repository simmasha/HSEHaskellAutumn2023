{- 
Задание 3: 
Напишите две реализации функции обращения списка reverse :: [a] -> [a] через свёртки foldr и foldl:

reverse' :: [a] -> [a]
reverse' = foldr fun' ini'
fun' = undefined
ini' = undefined

reverse'' :: [a] -> [a]
reverse'' = foldl fun'' ini''
fun'' = undefined
ini'' = undefined
-}

reverse' :: [a] -> [a]
reverse' = foldr fun' ini'
fun' c b = b ++ [c]
ini' = []

reverse'' :: [a] -> [a]
reverse'' = foldl fun'' ini''
fun'' b c = c : b
ini'' = []
