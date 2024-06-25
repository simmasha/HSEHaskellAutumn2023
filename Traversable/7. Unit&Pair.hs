{- 
Задание 7: 
Покажите, что всякий аппликативный функтор моноидален. Для этого реализуйте функции

unit' :: Applicative f => f ()
unit' = undefined

pair' :: Applicative f => f a -> f b -> f (a,b)
pair' = undefined
с сохранением подходящей семантики.
-}

unit' :: Applicative f => f ()
unit' = pure ()

pair' :: Applicative f => f a -> f b -> f (a,b)
pair' x y = fmap (,) x <*> y
