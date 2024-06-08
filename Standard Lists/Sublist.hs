{- 
Задание 5: 
Из заданного списка выделите подсписок, состоящий из элементов с n-го номера по k-ый, считая от нуля. При этом n-ый элемент должен входить в результат, а k-ый - нет.

sublist :: Int -> Int -> [a] -> [a]
sublist = undefined
Постарайтесь обеспечить разумное поведение для произвольных целых индексов и для бесконечных списков.

GHCi> sublist 2 5 "abcdefgh"
"cde"
GHCi> sublist 5 2 "abcdefgh"
""
-}
sublist :: Int -> Int -> [a] -> [a]
sublist n' k' l = take (k-n) (drop n l)
    where
        n = max 0 n'
        k = max 0 k'
