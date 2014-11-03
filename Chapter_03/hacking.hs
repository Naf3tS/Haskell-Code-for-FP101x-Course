abs' :: Int -> Int
abs' n = if n>=0 then n else -n

signum' :: Int -> Int
signum' n = if n<0 then -1 else
				if n==0 then 0 else 1

signum2' :: Int -> Int
signum2' n = if n<0 then
				-1
			 else
			 	if n==0 then
			 		0
			 	else
			 		1

-- Guarded equations:
abs2' :: Int -> Int
abs2' n | n>=0 = n
		| otherwise = -n

signum3' :: Int -> Int
signum3' n	| n>0 = 1
		  	| n<0 = -1
		  	| otherwise =0

-- Pattern matching:
not' :: Bool -> Bool
not' True = False
not' False = True

(&&&) :: Bool -> Bool -> Bool
True &&& True = True
_ &&& _ = False

-- More efficient as it avoids evaluating the second argument in the first pattern
(&&&&) :: Bool -> Bool -> Bool
True &&&& b = b
False &&&& _ = False

head' :: [a] -> a
head' (x:_) = x

tail' :: [a] -> [a]
tail' (_:xs) = xs

const1 :: a -> b -> a
const1 x _ = x

-- Use a lambda function to describe curried function more naturally
const2 :: a -> (b -> a)
const2 x = \_ -> x

odds1 n =	map f [0..n-1]
			where
				f x = x*2 + 1

odds2 n = map (\x -> x*2 + 1) [0..n-1]


-- Conditional expression:
safetail1 :: [a] -> [a]
safetail1 xs = if null xs then [] else tail xs

-- Guarded equation:
safetail2 :: [a] -> [a]
safetail2 xs	| null xs = []
				| otherwise = tail xs

-- Pattern matching:
safetail3 :: [a] -> [a]
safetail3 (x:xs)  = xs
safetail3 _ = []

-- Version 1:
(|||) :: Bool -> Bool -> Bool
(|||) True _ = True
(|||) _ True = True
(|||) _ _ = False

-- Version 2:
(||||) :: Bool -> Bool -> Bool
(||||) False False = False
(||||) _ _ = True

-- Version 3:
(|||||) :: Bool -> Bool -> Bool
(|||||) False b = b
(|||||) _ _ = True

-- Patterns
(&&&&&) :: Bool -> Bool -> Bool
(&&&&&) True True = True
(&&&&&) _ _ = False

-- Conditionals:
(&&&&&&) :: Bool -> Bool -> Bool
(&&&&&&) x y =	if x then
				if y then True else False
			else False


-- Patterns
(&&&&&&&) :: Bool -> Bool -> Bool
(&&&&&&&) True b = b
(&&&&&&&) False _ = False

-- Conditionals
(&&&&&&&&) :: Bool -> Bool -> Bool
(&&&&&&&&) x y = if x then y else False