{- 
Задание 2: 
Следующий тип данных задает гомогенную тройку элементов, которую можно рассматривать как трехмерный вектор:

data Triple a = Tr a a a  deriving (Eq,Show)
Сделайте этот тип функтором и аппликативным функтором с естественной для векторов семантикой, подобной ZipList.

GHCi> (^2) <$> Tr 1 (-2) 3
Tr 1 4 9
GHCi> Tr (^2) (+2) (*3) <*> Tr 2 3 4
Tr 4 5 12
-}
data Triple a = Tr a a a deriving (Show, Eq)


instance Functor Triple where
  -- fmap :: (a -> b) -> f a -> f b
  fmap = (<*>) . pure


instance Applicative Triple where
  -- pure :: a -> f a
  pure a = Tr a a a

  -- (<*>) :: f (a -> b) -> f a -> f b
  (Tr f g h) <*> (Tr a b c) = Tr (f a) (g b) (h c)
