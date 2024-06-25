{- 
Задание 3: 
Разложите положительное целое число на два сомножителя всевозможными способами, используя монаду списка и do-нотацию:

factor2 :: Integer -> [(Integer, Integer)]
factor2 n = do undefined
Пары должны быть уникальными, первый элемент пары не должен превышать второй, результат следует упорядочить лексикографически, в возрастающем порядке.

GHCi> factor2 45
[(1,45),(3,15),(5,9)]
-}
isfactor x n = n `mod` x == 0
factor2 :: Integer -> [(Integer, Integer)]
factor2 n = do 
    let y = toInteger $ floor $ sqrt $ fromInteger n
    x <- [1..y]
    True <- return $ isfactor x n
    [(,) x (n `div` x)]
