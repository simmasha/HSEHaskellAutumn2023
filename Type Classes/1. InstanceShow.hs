{- 
Задание 1: 
Сделайте тип

newtype Matrix a = Matrix [[a]]
представителем класса типов Show. Строки матрицы (внутренние списки) должны изображаться как списки; каждый следующий внутренний список должен начинаться с новой строки (используйте символ'\n' в качестве разделителя). Пустая матрица должна выводиться как EMPTY.

GHCi> Matrix [[1,2,3],[4,5,6],[7,8,9]]
[1,2,3]
[4,5,6]
[7,8,9]
GHCi> Matrix []
EMPTY
Не забывайте про существование полезных вспомогательных функций showChar, showString и showList.
-}

newtype Matrix a = Matrix [[a]]

instance Show a => Show (Matrix a) where
  show (Matrix []) = "EMPTY"
  show (Matrix rows) = showRows rows
    where
      showRow row = showList row ""
      showRows [] = ""
      showRows [row] = showRow row
      showRows (row:rows) = showRow row ++ "\n" ++ showRows rows
