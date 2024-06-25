{- 
Задание 1: 
Используя монаду Writer, напишите функцию правой свертки вычитанием

minusLoggedR :: (Show a, Num a) => a -> [a] -> Writer String a
minusLoggedR = undefined
в которой рекурсивные вызовы сопровождались бы записью в лог, так чтобы в результате получалось такое поведение:

GHCi> runWriter $ minusLoggedR 0 [1..3]
(2,"(1-(2-(3-0)))")
-}
import Control.Monad.Writer 
minusLoggedR :: (Show a, Num a) => a -> [a] -> Writer String a
minusLoggedR y [] = do
    tell $ show y
    return y
minusLoggedR y (x:xs) = do
    tell $ "(" ++ show x ++ "-"
    minusLoggedR y xs
    tell ")"
    -- tell $ "(" ++ show x ++ "-"
    -- tell ("-" ++ show y ++ join (replicate (length xs) ")"))
    return (foldr (-) y (x:xs)) 
