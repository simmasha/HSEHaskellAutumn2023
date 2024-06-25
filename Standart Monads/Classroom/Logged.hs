{- 
Задание: 
Следующий тип данных

data Logged a = Logged String a deriving (Eq,Show)
удобно использовать для записи в строковой лог в процессе вычислений. Фактически это упрощенная версия монады Writer, однако с инвертированным порядком записи в лог, см. пример ниже. Сделайте тип Logged представителем класса типов Monad. Реализуйте также функцию

write2log :: String -> Logged ()
write2log = undefined
позволяющую пользователю осуществлять запись в лог в процессе вычисления в монаде Logged. Для проверки используйте следующий код

logIt v = do
  write2log $ "var = " ++ show v ++ "; "
  return v

test = do
  x <- logIt 3
  y <- logIt 5
  let res = x + y
  write2log $ "sum = " ++ show res ++ "; "
  return res
который при запуске должен дать

GHCi> test
Logged "sum = 8; var = 5; var = 3; " 8
Обратите внимание на обратный порядок записи в лог по сравнению с библиотечным Writer'ом.
-}

import Control.Monad (ap)
-- Рукописный логгер
data Logged a = Logged String a deriving (Eq,Show)

instance Functor Logged where
  fmap f (Logged s a) = Logged s (f a)

instance Applicative Logged where
  pure = Logged ""
  (<*>) = ap

instance Monad Logged where
  return = pure
  (>>=) (Logged s a) f = Logged (s' ++ s) y
                        where
                            Logged s' y = f a

-- эквивалент tell
write2log :: String -> Logged ()
write2log str = Logged str ()

logIt v = do
    write2log $ "var = " ++ show v ++ "; "
    return v

test = do
    x <- logIt 3
    y <- logIt 5
    let res = x + y
    write2log $ "sum = " ++ show res ++ "; "
    return res
