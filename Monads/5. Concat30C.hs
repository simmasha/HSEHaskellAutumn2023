{- 
Задание 5: 
Для типа данных

data OddC a = Un a | Bi a a (OddC a) deriving (Eq,Show)
(контейнер-последовательность, который по построению может содержать только нечетное число элементов) реализуйте функцию

concat3OC :: OddC a -> OddC a -> OddC a -> OddC a
конкатенирующую три таких контейнера в один:

GHCi> tst1 = Bi 'a' 'b' (Un 'c')
GHCi> tst2 = Bi 'd' 'e' (Bi 'f' 'g' (Un 'h'))
GHCi> tst3 = Bi 'i' 'j' (Un 'k')
GHCi> concat3OC tst1 tst2 tst3
Bi 'a' 'b' (Bi 'c' 'd' (Bi 'e' 'f' (Bi 'g' 'h' (Bi 'i' 'j' (Un 'k')))))
Обратите внимание, что соображения четности запрещают конкатенацию двух контейнеров OddC.
-}
concat3OC :: OddC a -> OddC a -> OddC a -> OddC a
concat3OC (Un a) (Un b) w = Bi a b w
concat3OC (Bi a b c) y z = Bi a b (concat3OC c y z) 
concat3OC (Un a) (Bi x y z) w = Bi a x (concat3OC (Un y) z w)
