{- 
Задание 1: 
Для типа данных Type, кодирующего тип для просто типизированного лямбда-исчисления

type Symb = String 

infixr 3 :->
data Type = TVar Symb 
          | Type :-> Type
    deriving (Eq,Show)
реализуйте функции, возвращающие арность и порядок типа

arity :: Type -> Int
arity = undefined

order :: Type -> Int
order = undefined
Проверка

GHCi> [a,b,c] = map (TVar . pure) "abc"
GHCi> arity $ (b :-> c) :-> (a :-> b) :-> a :-> c
3
GHCi> order $ (b :-> c) :-> (a :-> b) :-> a :-> c
2
-}

type Symb = String 

infixr 3 :->
data Type = TVar Symb 
          | Type :-> Type
    deriving (Eq,Show)

arity :: Type -> Int
arity (TVar _) = 0
arity (_ :-> next) = 1 + arity next

order :: Type -> Int
order (TVar _) = 0
order (a :-> b) = max (order a + 1) (order b)
