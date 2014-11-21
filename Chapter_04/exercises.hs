pyths :: Int -> [(Int,Int,Int)]
pyths n = [(x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], x*x + y*y == z*z]


factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], sum (init(factors x)) == x]


scalarprod :: [Int] -> [Int] -> Int
scalarprod xs ys = sum([x*y | (x,y) <- zip xs ys])