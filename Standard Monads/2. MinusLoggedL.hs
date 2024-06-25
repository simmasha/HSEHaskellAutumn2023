{- 
Задание 2: 
Используя монаду Writer, напишите функцию левой свертки вычитанием

minusLoggedL :: (Show a, Num a) => a -> [a] -> Writer String a
minusLoggedL = undefined
в которой рекурсивные вызовы сопровождались бы записью в лог, так чтобы в результате получалось такое поведение:

GHCi> runWriter $ minusLoggedL 0 [1..3]
(-6,"(((0-1)-2)-3)")
-}
import Control.Monad.Writer
minusLoggedL :: (Show a, Num a) => a -> [a] -> Writer String a
reversed :: (Show a, Num a) => a -> [a] -> Writer String a
minusLoggedL y xs = reversed y (reverse xs) 
reversed y [] = do
    tell $ show y
    return y
reversed y (x:xs) = do
    tell "("
    reversed y xs
    tell $ "-" ++ show x ++ ")"
    return (foldl (-) y (x:xs))
