{- 
Задание 3: 
Следующий рекурсивный тип данных задает бинарное дерево:

data Tree a = Leaf | Node (Tree a) a (Tree a) 
  deriving Show
Напишите функции, которые вычисляют сумму элементов дерева и максимальную высоту дерева:

treeSum :: Tree Integer -> Integer
treeSum = undefined

treeHeight :: Tree a -> Int
treeHeight = undefined
GHCi> let tree = Node (Node (Node Leaf 1 Leaf) 2 Leaf) 3 (Node Leaf 4 Leaf)
GHCi> (treeSum tree, treeHeight tree)
(10,3)
-}

data Tree a = Leaf | Node (Tree a) a (Tree a) 
  deriving Show

treeSum :: Tree Integer -> Integer
treeSum Leaf = 0
treeSum (Node left value right) = value + treeSum left + treeSum right

treeHeight :: Tree a -> Int
treeHeight Leaf = 0
treeHeight (Node left _ right) = 1 + max (treeHeight left) (treeHeight right)
