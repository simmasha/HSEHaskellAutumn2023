{- 
Задание: 
Имеется игра на некотором поле, конфигурация поля задается типом Board. Имеется функция next :: Board -> [Board], которая по заданной конфигурации поля возвращает все конфигурации, достижимые из исходной за один ход. Несложно реализовать функции, возвращающие конфигурации, достижимые за 2 и за 3 хода:

twoTurns :: Board -> [Board]
twoTurns ini = do
  bd1 <- next ini
  next bd1
  
threeTurns :: Board -> [Board]
threeTurns ini = do
  bd1 <- next ini
  bd2 <- next bd1
  next bd2
Напишите функцию, которая возвращает всевозможные конфигурации доски через n ходов:

doNTurns :: Int -> Board -> [Board]
doNTurns n ini = undefined
Конфигурация должна входить в результирующий список столько раз, сколькими разными способами она может быть получена.
-}

-- тип Board и функция next :: Board -> [Board] определены в вызывающем коде
doNTurns :: Int -> Board -> [Board]
doNTurns 0 ini = [ini]
doNTurns n ini = do
        x <- next ini
        doNTurns (n - 1) x
