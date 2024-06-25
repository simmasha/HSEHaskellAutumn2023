{- 
Задание 8: 
Покажите, что всякий моноидальный функтор аппликативен. Для этого реализуйте функции

pure' :: Monoidal f => a -> f a
pure' = undefined

ap' :: Monoidal f => f (a -> b) -> f a -> f b
ap' = undefined
с сохранением подходящей семантики.
-}

pure' :: Monoidal f => a -> f a
pure' x = fmap ((\x y -> x) x) unit 

ap' :: Monoidal f => f (a -> b) -> f a -> f b
ap' f x =  uncurry ($) <$> (f *&* x)
