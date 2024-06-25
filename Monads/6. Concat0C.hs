{- 
Задание 6: 
Для типа данных

data OddC a = Un a | Bi a a (OddC a) deriving (Eq,Show)
реализуйте функцию

concatOC :: OddC (OddC a) -> OddC a
Она должна обеспечивать для типа OddC поведение, аналогичное поведению функции concat для списков:

GHCi> concatOC $ Un (Un 42)
Un 42
GHCi> tst1 = Bi 'a' 'b' (Un 'c')
GHCi> tst2 = Bi 'd' 'e' (Bi 'f' 'g' (Un 'h'))
GHCi> tst3 = Bi 'i' 'j' (Un 'k')
GHCi> concatOC $ Bi tst1 tst2 (Un tst3)
Bi 'a' 'b' (Bi 'c' 'd' (Bi 'e' 'f' (Bi 'g' 'h' (Bi 'i' 'j' (Un 'k')))))
-}
concatOC :: OddC (OddC a) -> OddC a
concatOC (Un a) = a
concatOC (Bi (Un x) (Un y) z) = Bi x y (concatOC z)
concatOC (Bi (Un x) (Bi y z a) k) = Bi x y (concatOC (Bi (Un z) a k))
concatOC (Bi (Bi x y z) e k) = Bi x y (concatOC (Bi z e k))
