{- 
Задание 1: 
Используя unfoldr, реализуйте функцию, которая возвращает в обратном алфавитном порядке список символов, попадающих в заданный парой диапазон. Попадание символа x в диапазон пары (a,b) означает, что x >= a и x <= b.

revRange :: (Char,Char) -> [Char]
revRange = unfoldr fun

fun = undefined
GHCi> revRange ('a','f')
"fedcba"
-}

import Data.List (unfoldr)
revRange :: (Char,Char) -> [Char]
revRange = unfoldr fun

fun (f, t) = if f > t then Nothing else Just (t, (f, pred t))
