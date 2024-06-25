{- 
Задание 1: 
Введём тип данных для представления ошибки обращения к списку по недопустимому индексу:

data ListIndexError = 
  ErrTooLargeIndex Int 
  | ErrNegativeIndex 
  | OtherErr String
  deriving (Eq, Show)
Реализуйте оператор (!!!) доступа к элементам массива по индексу, отличающийся от стандартного (!!) поведением в исключительных ситуациях. В этих ситуациях он должен выбрасывать подходящее исключение типа ListIndexError.

infixl 9 !!!
(!!!) :: MonadError ListIndexError m => [a] -> Int -> m a
xs !!! n = undefined
GHCi> let Right x = [1,2,3] !!! 0 in x
1
GHCi> let Left e = [1,2,3] !!! 42 in e
ErrTooLargeIndex 42
GHCi> let Left e = [1,2,3] !!! (-10) in e
ErrNegativeIndex
-}

{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
import Control.Monad.Except (MonadError)
import Control.Monad.Error (MonadError(throwError))

data ListIndexError
  = ErrTooLargeIndex Int
  | ErrNegativeIndex
  | OtherErr String
  deriving (Eq, Show)

helper :: Int -> [a] -> Bool
helper 0 _ = True
helper _ [] = False
helper n (_ : ys) = helper (n - 1) ys

infixl 9 !!!

(!!!) :: MonadError ListIndexError m => [a] -> Int -> m a
xs !!! n    | n < 0              = throwError ErrNegativeIndex
            | helper (n + 1) xs  = return (xs !! n)
            | otherwise          = throwError $ ErrTooLargeIndex n
