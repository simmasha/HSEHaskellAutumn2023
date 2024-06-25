{- 
Задание 1: 
Следующий рекурсивный тип данных задает двоичное дерево:

data Tree a = Leaf 
            | Node (Tree a) a (Tree a)
  deriving Show
Реализуйте функцию elemTree, определяющую, хранится ли заданное значение в заданном дереве.

GHCi> elemTree 1 t1
False
GHCi> elemTree 2 t1
True
GHCi> elemTree 42 (tInf1 3)
True
-}

data Tree a = Leaf | Node (Tree a) a (Tree a) 
  deriving (Show, Eq)

elemTree :: (Eq a) => a -> Tree a -> Bool
elemTree v = bfs . pure
    where
        bfs [] = False
        bfs roots = anyLeaf || bfs (concatMap (\(Node l _ r) -> [l, r]) $ filter (/= Leaf) roots)
            where
                anyLeaf = any ((Just v ==) . toMaybeVal) roots
                toMaybeVal Leaf = Nothing
                toMaybeVal (Node _ x _) = Just x
