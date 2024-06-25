{- 
Задание 6: 
Напишите представителей моноидального функтора для Maybe, пары и ((->) e):

GHCi> Just 3 *&* Just 5
Just (3,5)
GHCi> Just 3 *&* Nothing
Nothing
GHCi> ("This is ",3) *&* ("a pair!",5)
("This is a pair!",(3,5))
GHCi> (^2) *&* (*2) $ 5
(25,10)
-}

class Functor f => Monoidal f where
    unit  :: f ()
    (*&*) :: f a -> f b -> f (a,b)

instance Monoidal Maybe where
    unit = Just ()
    (*&*) Nothing _ = Nothing
    (*&*) _ Nothing = Nothing
    (*&*) (Just a) (Just b) = Just (a, b)

instance Monoid s => Monoidal ((,) s) where
    unit = (mempty, ())
    (*&*) (a, b) (c, d) = (a <> c, (b, d))

instance Monoidal ((->) e) where
    unit e = ()
    (*&*) a b e = (a e, b e)
