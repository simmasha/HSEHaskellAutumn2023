{- 
Задание 5: 
Найдите все сочетания по заданному числу элементов из заданного списка.

comb :: Int -> [a] -> [[a]]
comb = undefined
Например,

GHCi> comb 3 "abcde"
["abc","abd","abe","acd","ace","ade","bcd","bce","bde","cde"]
GHCi> take 4 $ comb 3 [1..]
[[1,2,3],[1,2,4],[1,2,5],[1,2,6]]
-}

comb :: Int -> [a] -> [[a]]
comb 0 _ = [[]] 
comb _ [] = []
comb n (x:xs) = map (x:) (comb (n-1) xs) ++ comb n xs
