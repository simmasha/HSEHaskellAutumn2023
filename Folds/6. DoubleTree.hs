{- 
Задание 6: 
Для реализации свертки двоичных деревьев нужно выбрать алгоритм обхода узлов дерева (см., например, http://en.wikipedia.org/wiki/Tree_traversal).

Сделайте двоичное дерево

data Tree a = Nil | Branch (Tree a) a (Tree a)  ﻿ deriving (Eq, Show)
представителем класса типов Foldable, реализовав симметричную стратегию (in-order traversal). Реализуйте также три другие стандартные стратегии (pre-order traversal, post-order traversal и level-order traversal), упаковав дерево в типы-обертки

newtype Preorder a   = PreO   (Tree a)  ﻿  deriving (Eq, Show)
newtype Postorder a  = PostO  (Tree a)   ﻿ deriving (Eq, Show)
newtype Levelorder a = LevelO (Tree a)    ﻿deriving (Eq, Show)
и сделав эти обертки представителями класса Foldable.

GHCi> let tree = Branch (Branch Nil 1 (Branch Nil 2 Nil)) 3 (Branch Nil 4 Nil)
GHCi> foldr (:) [] tree
[1,2,3,4]
GHCi> foldr (:) [] $ PreO tree
[3,1,2,4]
GHCi> foldr (:) [] $ PostO tree
[2,1,4,3]
GHCi> foldr (:) [] $ LevelO tree
[3,1,4,2]
Постарайтесь обеспечить пристойную эффективность.
-}

data Tree a = Nil | Branch (Tree a) a (Tree a)  deriving (Eq, Show)

newtype Preorder a = PreO (Tree a) deriving (Eq, Show)
newtype Postorder a = PostO (Tree a) deriving (Eq, Show)
newtype Levelorder a = LevelO (Tree a) deriving (Eq, Show)

instance Foldable Tree where
  foldMap f t = inorder f t
    where inorder f Nil            = mempty
          inorder f (Branch l a r) = inorder f l `mappend` f a `mappend` inorder f r

instance Foldable Preorder where 
  foldMap f (PreO t) = preorder f t
    where preorder f Nil            = mempty
          preorder f (Branch l a r) = f a `mappend` preorder f l `mappend` preorder f r

instance Foldable Postorder where 
  foldMap f (PostO t) = postorder f t
    where postorder f Nil            = mempty
          postorder f (Branch l a r) = postorder f l `mappend` postorder f r `mappend` f a

instance Foldable Levelorder where 
  foldMap f (LevelO t) = levelorder f [t] [] mempty
    where levelorder f []                  [] acc = acc
          levelorder f []                  sk acc = levelorder f (reverse sk) [] acc
          levelorder f (Nil:xs)            sk acc = levelorder f xs sk acc
          levelorder f ((Branch l a r):xs) sk acc = levelorder f xs (r : l : sk) (acc `mappend` f a)
