{- 
Задание 7: 
Реализуйте функцию, находящую значение определённого интеграла от заданной функции на заданном интервале методом трапеций.

integration :: (Double -> Double) -> Double -> Double -> Double
integration f a b = undefined
Используйте равномерную сетку, достаточно 1000 элементарных отрезков.

GHCi> integration sin pi 0
-2.0
Результат может отличаться от -2.0, но не более чем на 1e-4.
-}
integration :: (Double -> Double) -> Double -> Double -> Double
integration f a b = h * ((f a + f b)/2 + (sum $ map f xs))
    where
        n = 1000
        h = (b-a)/n
        xs = map(\x -> a + h * x) [1..n-1]
