{- 
Задание 4: 
Реализуйте функцию, задающую циклическое вращение списка.

rotate :: Int -> [a] -> [a]
rotate n xs = undefined
При положительном значении целочисленного аргумента ротация должна осуществляться влево, при отрицательном - вправо.

GHCi> rotate 2 "abcdefghik"
"cdefghikab"
GHCi> rotate (-2) "abcdefghik" 
"ikabcdefgh"
GHCi> rotate 1000000001 [1..10]
[2,3,4,5,6,7,8,9,10,1]
Не забывайте обеспечить работоспособность вашей реализации на бесконечных списках (для сценариев, когда это имеет смысл).
-}

rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate n xs = case drop n xs of 
    [] -> helper (mod n $ length xs) xs
    _  -> helper n xs
    
helper _ [] = []
helper 0 xs = xs
helper n xs | n > 0 = drop n xs ++ take n xs
            | n < 0 = let shift = length xs - (-n) in helper shift xs
