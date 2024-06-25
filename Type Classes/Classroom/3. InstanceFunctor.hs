{- 
Задание 3: 
Следующий рекурсивный тип данных задает двоичное дерево:

data Tree a = Leaf 
            | Node (Tree a) a (Tree a)
  deriving Show
Сделайте тип дерева представителем класса типов Functor.

GHCi> fmap (^2) t1
Node (Node (Node Leaf 25 Leaf) 4 Leaf) 9 (Node Leaf 16 Leaf)
-}

data Tree a = Leaf | Node (Tree a) a (Tree a) 
  deriving Show

instance Functor Tree where
  fmap _ Leaf = Leaf
  fmap f(Node left x right) = Node (fmap f left) (f x) (fmap f right)
