module Chapter_07 where

import Data.Char

type Parser a = String -> [(a,String)]

-- Basic parsers:
return' :: a -> Parser a
return' v = \inp -> [(v,inp)]

failure :: Parser a
failure = \inp -> []

item :: Parser Char
item = \inp -> case inp of
	[] -> []
	(x:xs) -> [(x,xs)]

-- Parse application function:
parse :: Parser a -> String -> [(a,String)]
parse p inp = p inp


-- Sequencing
-- A parser that consumes 3 characters:
-- p :: Parser (Char, Char)
p = do  x <- item
        item
        y <- item
        return' (x,y)

(+++) :: Parser a -> Parser a -> Parser a
p +++ q = \inp -> case parse p inp of
  [] -> parse q inp
  [(v,out)] -> [(v,out)]


-- sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return' x else failure

-- digit :: Parser Char
-- digit = sat isDigit