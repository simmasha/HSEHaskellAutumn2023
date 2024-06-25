{- 
Задание 2: 
Тип данных Person определен как запись:

data Person = Person { firstName :: String, lastName :: String, age :: Int }
        deriving Show
Реализуйте функцию abbrFirstName, которая сокращает имя до первой буквы с точкой, то есть если имя было "John", то после применения этой функции, оно превратится в "J.". Однако если имя было короче двух символов, то оно не меняется.

abbrFirstName :: Person -> Person
abbrFirstName p = undefined
GHCi> let p = Person {firstName = "Adam", lastName = "Smith", age = 66}
GHCi> abbrFirstName p
Person {firstName = "A.", lastName = "Smith", age = 66}
-}

data Person = Person { firstName :: String, lastName :: String, age :: Int }

abbrFirstName :: Person -> Person
abbrFirstName p@(Person { firstName = fn })
    | length fn <= 2 = p
    | otherwise = p { firstName = [head fn, '.'] }
