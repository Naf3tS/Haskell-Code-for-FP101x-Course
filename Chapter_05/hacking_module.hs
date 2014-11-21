-- No recursion:
factorial1 :: Int -> Int
factorial1 n = product [1..n]

-- Recursive version:
factorial2 :: Int -> Int
factorial2 0 = 1
factorial2 n = n * factorial2 (n-1)

-- Recursion on lists:
product1 :: [Int] -> Int
product1 [] = 1
product1 (x:xs) = x * product1 xs

length1 :: [a] -> Int
length1 (_:xs) = 1 + length1 xs

reverse1 :: [a] -> [a]
reverse1 [] = []
reverse1 (x:xs) = reverse xs ++ [x] -- Need [a] here instead of just a

zip1 :: [a] -> [b] -> [(a,b)]
zip1 [] _ = []
zip1 _ [] = []
zip1 (x:xs) (y:ys) = (x,y) : zip1 xs ys

drop1 :: Int -> [a] -> [a]
drop1 0 xs = xs
drop1 n (_:xs) = drop (n-1) xs

qsort1 :: Ord a => [a] -> [a]
qsort1 [] = []
qsort1 (x:xs) = qsort1 lessThan ++ [x] ++ qsort1 greaterThan
					where 
						lessThan = [a | a<-xs, a<=x]
						greaterThan = [b | b<- xs, b>x]

-- Exercise 0
and1 :: [Bool] -> Bool
and1 [True] = True
and1 (False:_) = False
and1 (True:xs) = and1 xs

concat1 :: [[a]] -> [a]
concat1 [] = []
concat1 (x:xs) = x ++ concat1 xs

replicate1 :: Int -> a -> [a]
replicate1 0 _ = []
replicate1 n x = x : replicate1 (n-1) x

(!!!) :: [a] -> Int -> a
(!!!) (x:_) 0 = x
(!!!) (_:xs) n = (!!!) xs (n-1)

elem1 :: Eq a => a -> [a] -> Bool
elem1  _  [] = False
elem1 k (x:xs) = if x==k then True else elem1 k xs

merge1 :: [Int] -> [Int] -> [Int]
merge1 [] ys = ys
merge1 xs [] = xs
merge1 (x:xs) (y:ys) = if x<=y then x : merge1 xs (y:ys) else y : merge1 (x:xs) ys

msort1 :: [Int] -> [Int]
msort1 [] = []
msort1 xs
	| length xs==1 = xs
	| otherwise = merge1 (msort1 firstHalf) (msort1 secondHalf)
		where
			n = length xs
			firstHalf = take (n `div` 2) xs
			secondHalf = drop (n `div` 2) xs