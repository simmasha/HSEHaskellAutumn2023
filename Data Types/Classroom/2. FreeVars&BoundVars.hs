{- 
Задание 2: 
Следующий тип данных может использоваться для кодирования термов чистого лямбда-исчисления, просто типизированного в стиле Черча

infixl 4 :@
data Term = Var Symb
          | Term :@ Term
          | Lam Symb Type Term
    deriving (Eq,Show)
Например, комбинатор 
ω=λ(x^a).xx в этом представлении будет иметь вид Lam "x" (Tvar "a") (Var "x" :@ Var "x").

Для типа данных Term реализуйте функции, возвращающие списки свободных и связанных переменных терма
freeVars :: Term -> [Symb]
freeVars = undefined
boundVars :: Term -> [Symb]
boundVars = undefined

Для последней функции нужно обеспечить для переменной такое количество вхождений в результирующий список, сколько раз эта переменная связана в терме.
GHCi> x = Var "x"
GHCi> a = TVar "a"
GHCi> boundVars $ Lam "x" a (x :@ x)
["x"]
GHCi> boundVars $ Lam "x" a (x :@ Lam "x" a x)
["x","x"]
GHCi> boundVars $ x :@ Lam "x" a (x :@ Lam "x" a x) 
["x","x"]
-}
import Data.List (nub)

type Symb = String 

infixr 3 :->
data Type = TVar Symb 
          | Type :-> Type
    deriving (Eq,Show)

infixl 4 :@
data Term = Var Symb
          | Term :@ Term
          | Lam Symb Type Term
    deriving (Eq,Show)

freeVars :: Term -> [Symb]
freeVars (Var x) = [x]
freeVars (t1 :@ t2) = nub (freeVars t1 ++ freeVars t2)
freeVars (Lam x _ t) = nub (filter (/= x) (freeVars t))

boundVars :: Term -> [Symb]
boundVars (Var _) = []
boundVars (t1 :@ t2) = boundVars t1 ++ boundVars t2
boundVars (Lam x _ t) = x : boundVars t
