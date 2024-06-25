{- 
Задание 3: 
Задавая контексты с помощью синонима типа

type Env = [(Symb,Type)]
реализуйте алгоритм вывода типа для комбинаторов в просто типизированном лямбда-исчислении в стиле Черча

infer0 :: Term -> Maybe Type
infer0 = infer [] 

infer :: Env -> Term -> Maybe Type
infer = undefined
Проверка

GHCi> [f,g,x] = map (Var . pure) "fgx"
GHCi> [a,b,c] = map (TVar . pure) "abc"
GHCi> infer0 (Lam "f" (b:->c) $ Lam "g" (a:->b) $ Lam "x" a $ f 
:@ (g :@ x))
Just ((TVar "b" :-> TVar "c") :-> ((TVar "a" :-> TVar "b") :-> 
(TVar "a" :-> TVar "c")))
GHCi> infer0 (Lam "f" b $ Lam "g" c $ Lam "x" a $ f :@ (g :@ x))
Nothing
-}

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

type Env = [(Symb,Type)]

infer0 :: Term -> Maybe Type
infer0 = infer [] 

infer :: Env -> Term -> Maybe Type
infer env (Var x) = lookup x env
infer env (t1 :@ t2) = case (infer env t1, infer env t2) of
  (Just (t1' :-> t), Just t2') | t1' == t2' -> Just t
  _ -> Nothing
infer env (Lam x t1 t2) = case infer ((x, t1) : env) t2 of
  Just t -> Just (t1 :-> t)
  _ -> Nothing
