{- 
Задание 5: 
Часто парсеры пишут в виде, допускающем разбор неоднозначных грамматик

newtype Parser a = Parser { apply :: String -> [(a, String)] }
Результатом разбора должен быть список возможных подходящих результатов:

GHCi> apply (many $ char 'A') "AA"
[("AA",""),("A","A"),("","AA")]
Удобно ввести функцию

parse :: Parser a -> String -> [a]
parse p = map fst . filter (null . snd) . apply p
Если парсер p не поглощает всю входную строку, то parse трактует такой частичный разбор, как неудачу

GHCi> parse (many $ char 'A') "AA"
["AA"]
Сделайте приведенный выше парсер представителем Applicative и Alternative, так чтобы обеспечить поведение, согласованное с возможностью неоднозначного разбора:

GHCi> twoChars x = (\a b -> [a,b]) <$> char x <*> char x
GHCi> threeChars x = (\a b c -> [a,b,c]) <$> char x <*> char x <*> char x
GHCi> parse (some (twoChars '7') <|> some (threeChars '7')) "777777"
[["77","77","77"],["777","777"]]
В примерах подразумевается, что парсер char :: Char -> Parser Char ведет себя совершенно стандартно, возвращая одноэлементный список на подходящем символе и пустой на прочих.
-}

import Control.Applicative

newtype Parser a = Parser { apply :: String -> [(a, String)] }

instance Functor Parser where
    fmap g p = Parser f
        where
            f s = case apply p s of
                [(x, xs)] -> [(g x, xs)]
                [] -> []

instance Applicative Parser where
    pure x = Parser (\s -> [(x, s)])
    (<*>) (Parser p1) (Parser p2) = Parser f
        where
            f xs = [(g x, xs1) | (g, xs2) <- p1 xs, (x, xs1) <- p2 xs2]

instance Alternative Parser where
    empty = Parser (const [])
    (<|>) (Parser p1) (Parser p2) = Parser f where
        f xs = case p1 xs of
            [] -> case p2 xs of
                [] -> []
                z  -> z
            z' -> z' ++ case p2 xs of
                []  -> []
                z'' -> z''
