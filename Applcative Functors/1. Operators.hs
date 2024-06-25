{- 
Задание 1: 
В модуле Data.List имеется семейство функций zipWith, zipWith3, zipWith4,..:

GHCi> [x1s,x2s,x3s,x4s] = [[1,2,3],[4,5,6],[7,8,9],[10,11,12]]
GHCi> zipWith (\a b -> 2*a+3*b) x1s x2s
[14,19,24]
GHCi> zipWith3 (\a b c -> 2*a+3*b+5*c) x1s x2s x3s
[49,59,69]
GHCi> zipWith4 (\a b c d -> 2*a+3*b+5*c-4*d) x1s x2s x3s x4s
[9,15,21]
Аппликативные функторы могут заменить всё это семейство

GHCi> getZipList $ (\a b -> 2*a+3*b) <$> ZipList x1s <*> ZipList x2s
[14,19,24]
GHCi> getZipList $ (\a b c -> 2*a+3*b+5*c) <$> ZipList x1s <*> ZipList x2s <*> ZipList x3s
[49,59,69]
GHCi> getZipList $ (\a b c d -> 2*a+3*b+5*c-4*d) <$> ZipList x1s <*> ZipList x2s <*>ZipList x3s <*> ZipList x4s
[9,15,21]
Реализуйте операторы (>*<) и (>$<), позволяющие спрятать упаковку ZipList и распаковку getZipList:

GHCi> (\a b -> 2*a+3*b) >$< x1s >*< x2s
[14,19,24]
GHCi> (\a b c -> 2*a+3*b+5*c) >$< x1s >*< x2s >*< x3s
[49,59,69]
GHCi> (\a b c d -> 2*a+3*b+5*c-4*d) >$< x1s >*< x2s >*< x3s >*< x4s
[9,15,21]

-}
import Control.Applicative (ZipList(ZipList), getZipList)

infixl 4 >$<, >*<

(>$<) :: (a -> b) -> [a] -> [b]
(>$<) f x = getZipList (f <$> ZipList x)

(>*<) :: [a -> b] -> [a] -> [b]
(>*<) x y = getZipList (ZipList x <*> ZipList y)
