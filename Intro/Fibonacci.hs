{- 
Задание 5: 
Понятие чисел Фибоначчи можно расширить, потребовав, чтобы рекуррентное соотношение выполнялось для произвольных целых значений аргумента, в том числе и отрицательных. Реализуйте функцию, вычисляющую числа Фибоначчи так, чтобы она удовлетворяла этому требованию.

fibonacci :: Integer -> Integer
fibonacci n = undefined
Реализация должна иметь сложность не хуже линейной по числу вызовов оператора сложения.

GHCi> fibonacci (-99)
218922995834555169026
-}
fibonacci :: Integer -> Integer
fibonacci n
  | n > 1 = helper n 1 0 1
  | n < 0 = if even n then (-(helper(-n) 1 0 1)) else helper (-n) 1 0 1
  | otherwise = n

helper :: Integer -> Integer -> Integer -> Integer -> Integer
helper n curIdx first second
    | curIdx == n = second
    | otherwise = helper n (curIdx + 1) second (first + second)
