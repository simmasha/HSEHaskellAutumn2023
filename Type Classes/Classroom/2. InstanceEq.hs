{- 
Задание 2: 
Следующий рекурсивный тип данных задает двоичное дерево:

data Tree a = Leaf 
            | Node (Tree a) a (Tree a)
  deriving Show
Сделайте тип дерева представителем класса типов Eq.

GHCi> t2 == t2
True
GHCi> t3 /= t2
True
GHCi> tInf1 3 /= tInf2 3
True
-}

data Tree a = Leaf | Node (Tree a) a (Tree a)
  deriving Show

instance (Eq a) => Eq (Tree a) where
  (/=) Leaf Leaf = False
  (/=) x y = bfsNeq [(x, y)]
    where
      bfsNeq [] = False
      bfsNeq pairs = anyNeq || bfsNeq (concatMap pushDown pairs)
        where
          anyNeq = any neq pairs
          neq (Leaf, Leaf) = False
          neq (Node _ x0 _, Node _ x1 _) = x0 /= x1
          neq _ = True
          pushDown (Leaf, Leaf) = []
          pushDown (Node l0 _ r0, Node l1 _ r1) = [(l0, l1), (r0, r1)]
