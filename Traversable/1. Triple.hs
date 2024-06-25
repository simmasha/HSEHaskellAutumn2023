{- 
Задание 1: 
Сделайте тип

data Triple a = Tr a a a  deriving (Eq,Show)
из предыдущего домашнего задания представителем классов типов Foldable и Traversable:

GHCi> foldl (++) "!!" (Tr "ab" "cd" "efg")
"!!abcdefg"
GHCi> traverse (\x -> if x>10 then Right x else Left x) (Tr 12 14 16)
Right (Tr 12 14 16)
GHCi> traverse (\x -> if x>10 then Right x else Left x) (Tr 12 8 4)
Left 8
GHCi> sequenceA (Tr (Tr 1 2 3) (Tr 4 5 6) (Tr 7 8 9))
Tr (Tr 1 4 7) (Tr 2 5 8) (Tr 3 6 9)
Поскольку последний тест в приведенном выше списке подразумевает, что тип Triple является также представителем класса типов Applicative, скопируйте необходимые реализации представителей из предыдущего домашнего задания.
-}

import Data.Traversable (foldMapDefault)
data Triple a = Tr a a a deriving (Eq,Show)
instance Functor Triple where  
  fmap g (Tr x y z) = Tr (g x) (g y) (g z)
instance Applicative Triple where
  pure a = Tr a a a 
  Tr x y z <*> Tr a b c = Tr (x a) (y b) (z c)
instance Foldable Triple where
  foldMap = foldMapDefault
instance Traversable Triple where
  traverse g (Tr a b c) = Tr <$> g a <*> g b <*> g c
