{- 
Задание 4: 
Реализуйте функцию, находящую элементы следующей рекуррентной последовательности
b0 = 1; b1​ = 2; b2​ =3; b(k+3)​ = b(k+2)​−2b(k+1)​+3bk​

seqB :: Integer -> Integer
seqB n = undefined

Попытайтесь найти эффективное решение со сложностью не хуже линейной по числу вызовов арифметических операторов.
GHCi> seqB 42
-13122927
-}
seqB :: Integer -> Integer
seqB 0 = 1
seqB 1 = 2
seqB 2 = 3
seqB n = go (n-3) 1 2 3
    where
        go :: Integer -> Integer -> Integer -> Integer -> Integer
        go k bk bk1 bk2
            | k > 0 = go (k-1) bk1 bk2 bk3
            | k == 0 = bk3
            where
                bk3 = bk2 - 2*bk1 +3*bk
