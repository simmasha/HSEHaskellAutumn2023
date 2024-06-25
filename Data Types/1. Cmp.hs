{- 
Задание 1: 
Тип данных Ordering определен в стандартной библиотеке так:

data Ordering = LT | EQ | GT
Он используется при определении функций, сравнивающих элементы каких-либо типов. Если первый аргумент меньше второго, возвращается LT, если равен - EQ, если больше - GT.
Определим тип LogLevel следующим образом

data LogLevel = Error | Warning | Info
Реализуйте функцию cmp, сравнивающую элементы типа LogLevel так, чтобы имел место порядок Error > Warning > Info.

cmp :: LogLevel -> LogLevel -> Ordering
cmp = undefined
-}

data LogLevel = Error | Warning | Info

cmp :: LogLevel -> LogLevel -> Ordering
cmp Error Error = EQ
cmp Warning Warning = EQ
cmp Info Info = EQ
cmp Error _ = GT
cmp _ Error = LT
cmp Warning _ = GT
cmp _ Warning = LT
