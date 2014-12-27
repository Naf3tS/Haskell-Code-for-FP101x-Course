module Chap_11 where

-- Exercise 6

fibs :: [Integer]
fibs = 0 : 1 : [x+y | (x,y) <- zip fibs (tail fibs)]


-- Exercise 7

fib :: Int -> Integer
fib n = fibs !! n


-- Exercise 8

largeFib :: Integer
largeFib = head (dropWhile (<=1000) fibs)


-- Exercise 9
repeat :: a -> [a]
repeat x = xs
    where xs = x:xs

data Tree a = Leaf | Node (Tree a) a (Tree a)
    deriving (Show)
repeatTree :: a -> Tree a
repeatTree x = Node t x t
    where t = repeatTree x
