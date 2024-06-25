{- 
Задание 4: 
Вычислите модули разностей между соседними элементами списка, используя монаду списка и do-нотацию:

absDiff :: Num a => [a] -> [a]
absDiff xs = do undefined
GHCi> absDiff [2,7,22,9]
[5,15,13]
-}

absDiff :: Num a => [a] -> [a]
absDiff xs = do 
    (x, y) <- zip xs (drop 1 xs)
    [abs (x-y)]
