-- Exercise 0
sum100 = sum [x^2 | x <- [1..100]] -- b

-- Exercise 1
replicate n a = [a | _ <- [1..a]] -- d


-- Exercise 2
pyths :: Int -> [(Int,Int,Int)]
pyths n = [(x,y,z) | x <- [1..n],  y <- [1..n], z <- [1..n], x*x + y*y == z*z] -- a


-- Exercise 3
factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], isPerfect x]
	where isPerfect num = sum (init(factors num)) == num


-- Exercise 5
find :: (Eq a) => a -> [(a,b)] -> [b]
find k t = [v | (k', v) <- t, k == k']

positionsX :: (Eq a) => a -> [a] -> [Int]
positionsX x xs = find x (zip xs [0..n]) where n = length xs - 1

positions1 :: (Eq a) => a -> [a] -> [Int]
positions1 x xs = find x (zip xs [0..n]) where n = length xs-1 -- a

-- positions2 :: (Eq a) => a -> [a] -> [Int]
-- positions2 x xs = find x xs

-- positions3 :: (Eq a) => a -> [a] -> [Int]
-- positions3 x xs = find x (zipWith (+) xs [0..n]) where n = length xs-1 -- b


-- Exercise 6
scalarproductX :: Num a => [a] -> [a] -> a
scalarproductX xs ys = sum [x * y | (x,y) <- zip xs ys]

scalarproduct1 :: Num a => [a] -> [a] -> a
scalarproduct1 xs ys = sum [x * y | x <- xs, y <- ys]

scalarproduct2 :: Num a => [a] -> [a] -> a
scalarproduct2 xs ys = sum [x * y | (x,y) <- xs `zip` ys]

scalarproduct3 :: Num a => [a] -> [a] -> a
scalarproduct3 xs ys = product (zipWith (+) xs ys)

-- scalarproduct4 :: Num a => [a] -> [a] -> a
-- scalarproduct4 xs ys = sum (product [(x,y) | x <- xs, y <- ys])


-- Exercise 11
xs = 1 : [x+1 | x<-xs]


-- Exercise 12
-- riffleX [1,2,3] [4,5,6] = [1,4,2,5,3,6]
riffleX :: [a] -> [a] -> [a]
riffleX xs ys = concat [ x:y:[] | (x,y)<-zip xs ys] 

riffle2 :: [a] -> [a] -> [a]
riffle2 xs ys = concat [ [x,y] | (x,y)<- xs `zip` ys] 


-- Exercise 13
divisors :: Int -> [Int]
divisors x = [d | d<-[1..x], x `divides` d]

divides :: Int -> Int -> Bool
divides n d = (n `mod` d) == 0