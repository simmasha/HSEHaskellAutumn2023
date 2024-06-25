{- 
Задание 3: 
Сделайте двоичное дерево

data Tree a = Nil | Branch (Tree a) a (Tree a)   deriving (Eq, Show)
функтором и аппликативным функтором, реализовав в последнем случае семантику применения узла к соответствующему узлу второго дерева:

GHCi> let t1 = Branch (Branch Nil 7 Nil) 2 Nil
GHCi> let t2 = Branch (Branch Nil 3 Nil) 4 (Branch Nil 5 Nil)
GHCi> (*) <$> t1 <*> t2
Branch (Branch Nil 21 Nil) 8 Nil
GHCi> Branch (Branch Nil (+3) Nil) (*2) Nil <*> Branch Nil 7 (Branch Nil 5 Nil)
Branch Nil 14 Nil
-}

data Tree a = Nil | Branch (Tree a) a (Tree a) deriving (Eq, Show)

instance Functor Tree where
    fmap _ Nil = Nil
    fmap f (Branch tl a tr) = Branch (fmap f tl) (f a) (fmap f tr)

instance Applicative Tree where
    pure a = Branch (pure a) a (pure a)

    (<*>) Nil _ = Nil
    (<*>) _ Nil = Nil
    (<*>) (Branch fTl f fTr) (Branch tl x tr) = Branch (fTl <*> tl) (f x) (fTr <*> tr)
