{- 
Задание 2: 
Сделайте двоичное дерево

data Tree a = Nil | Branch (Tree a) a (Tree a)   deriving (Eq, Show)
представителем класса типов Traversable:

GHCi> traverse (\x -> if odd x then Right x else Left x) (Branch (Branch Nil 1 Nil) 3 Nil)
Right (Branch (Branch Nil 1 Nil) 3 Nil)
GHCi> traverse (\x -> if odd x then Right x else Left x) (Branch (Branch Nil 1 Nil) 2 Nil)
Left 2
GHCi> sequenceA $ Branch (Branch Nil [1,2] Nil) [3] Nil
[Branch (Branch Nil 1 Nil) 3 Nil,Branch (Branch Nil 2 Nil) 3 Nil]
-}

data Tree a = Nil | Branch (Tree a) a (Tree a) deriving (Eq, Show)

instance Foldable Tree where
    foldr _ z Nil = z
    foldr f z (Branch tl a tr) = foldr f res tl
        where
            res = f a res'
            res' = foldr f z tr

instance Functor Tree where
    fmap _ Nil = Nil
    fmap f (Branch tl a tr) = Branch (fmap f tl) (f a) (fmap f tr)

instance Applicative Tree where
    pure a = Branch (pure a) a (pure a)

    (<*>) Nil _ = Nil
    (<*>) _ Nil = Nil
    (<*>) (Branch fTl f fTr) (Branch tl x tr) = Branch (fTl <*> tl) (f x) (fTr <*> tr)

instance Traversable Tree where
    traverse _ Nil = pure Nil
    traverse f (Branch tl v tr) = Branch <$> traverse f tl <*> f v <*> traverse f tr

    sequenceA = traverse id
