{- 
Задание 3: 
Реализуйте на основе библиотеки парсеров, разработанной на лекции, парсер

nat :: Parser Char Int
nat = undefined
обеспечивающий следующее поведение

GHCi> runParser nat "123ASSD"
Just ("ASSD",123)
GHCi> multiplication2 = (*) <$> nat <*  char '*' <*> nat
GHCi> runParser multiplication2 "14*30"
Just ("",420)
Тип данных Parser, представители классов типов Functor, Applicative и Alternative для него и конкретные парсеры из лекции (char, satisfy и т.д.) уже присутствуют в вызывающем коде, не нужно их реализовывать повторно.
-}

nat :: Parser Char Int
nat = read . concatMap show <$> some digit
