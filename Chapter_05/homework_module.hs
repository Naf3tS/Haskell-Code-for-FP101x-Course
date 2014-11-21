-- Exercise 0
m ^^^ 0 = 1
-- m ^^^ n = m * (^) m (n-1)
m ^^^ n = m * m * m ^ (n-1)


-- Exercise 1
length' :: [a] -> Int
length' [] = 0
length' (_:xs) = 1 + length' xs


-- Exercise 2
drop' :: Int -> [a] -> [a]
drop' 0 xs = xs
drop' n [] = []
drop' n (_:xs) = drop' (n-1) xs


-- Exercise 3
init' :: [a] -> [a]
init' [_] = []
init' (x:xs) = x : init xs


-- Exercise 4
and' :: [Bool] -> Bool
and' [] = True
-- and' (b:bs) = b && and' bs
{-
and' (b:bs)
	| b = and bs
	| otherwise = False
-}
{-
and' (b:bs)
	| b == False = False
	| otherwise = and bs
-}
and' (b:bs) = and' bs && b


-- Exercise 5
concat' :: [[a]] -> [a]
concat' [] = []
concat' (xs:xss) = xs ++ concat' xss


-- Exercise 6
replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x = x : replicate' (n-1) x


-- Exercise 7
(!!!) :: [a] -> Int -> a
(x:_) !!! 0 = x
(_:xs) !!! n = xs !!! (n-1)


-- Exercise 8
elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' x (y:ys)
	| x == y = True
	| otherwise = elem' x ys


-- Exercise 9
merge' :: Ord a => [a] -> [a] -> [a]
merge' [] ys = ys
merge' xs [] = xs
merge' (x:xs) (y:ys) = if x<=y then x : merge' xs (y:ys) else y : merge' (x:xs) ys


-- Exercise 10
halve' :: [a] -> ([a],[a])
halve' xs = splitAt (length  xs `div` 2) xs

msort' :: Ord a => [a] -> [a]
msort' [] = []
msort' [x] = [x]
msort' xs = merge' (msort' ys) (msort' zs)
	where (ys,zs) = halve' xs