-- Exercise 0
f0 :: (a -> b) -> (a -> Bool) -> [a] -> [b]
f0 f p xs = [f x | x <- xs, p x]

f0_1 :: (a -> b) -> (a -> Bool) -> [a] -> [b]
f0_1 f p xs = map f (filter p xs)


-- Exercise 1
all_1 :: (a -> Bool) -> [a] -> Bool
all_1 p xs = and (map p xs)

-- all_2 :: (a -> Bool) -> [a] -> Bool
-- all_2 p xs = map p (and xs)

all_3 :: (a -> Bool) -> [a] -> Bool
all_3 p = and . map p

-- A & B & C = !(!A | !B | !C)
all_4 :: (a -> Bool) -> [a] -> Bool
all_4 p = not . any (not . p)

-- all_5 :: (a -> Bool) -> [a] -> Bool
-- all_5 p = map p . and

all_6 :: (a -> Bool) -> [a] -> Bool
all_6 p xs = foldl (&&) True (map p xs)

all_7 :: (a -> Bool) -> [a] -> Bool
all_7 p xs = foldr (&&) False (map p xs)

all_8 :: (a -> Bool) -> [a] -> Bool
all_8 p = foldr (&&) True . map p


-- Exercise 2
-- any_1 :: (a -> Bool) -> [a] -> Bool
-- any_1 p = map p . or

any_2 :: (a -> Bool) -> [a] -> Bool
any_2 p = or . map p 

any_3 :: (a -> Bool) -> [a] -> Bool
any_3 p xs = length (filter p xs) > 0

any_4 :: (a -> Bool) -> [a] -> Bool
any_4 p = not . null . dropWhile (not . p)

any_5 :: (a -> Bool) -> [a] -> Bool
any_5 p = null . filter p

-- A | B | C = !(!A & !B & !C)
any_6 :: (a -> Bool) -> [a] -> Bool
any_6 p xs = not (all (\x -> not (p x)) xs)

any_7 :: (a -> Bool) -> [a] -> Bool
any_7 p xs = foldr (\x acc -> (p x) || acc) False xs

any_8 :: (a -> Bool) -> [a] -> Bool
any_8 p xs = foldr (||) True (map p xs)


-- Exercise 3
takeWhile_2 :: (a -> Bool) -> [a] -> [a]
takeWhile_2 _ [] = []
takeWhile_2 p (x:xs)
	| p x = x : takeWhile_2 p xs
	| otherwise = []

takeWhile_4 :: (a -> Bool) -> [a] -> [a]
takeWhile_4 p = foldl (\acc x -> if p x then x : acc else acc) []


-- Exercise 4
dropWhile_1 :: (a -> Bool) -> [a] -> [a]
dropWhile_1 _ [] = []
dropWhile_1 p (x:xs)
	| p x = dropWhile_1 p xs
	| otherwise = x:xs


-- Exercise 5
map_4 :: (a -> b) -> [a] -> [b]
map_4 f = foldl (\xs x -> xs  ++ [f x]) []


-- Exercise 6
filter_2 :: (a -> Bool) -> [a] -> [a]
filter_2 p = foldr (\x xs -> if p x then x:xs else xs) []


-- Exercise 7
dec2int_3 :: [Integer] -> Integer
dec2int_3 = foldl (\x y -> 10*x + y) 0


-- Exercise 8
compose' :: [a -> a] -> (a -> a)
compose' = foldr (.) id
-- sumsqreven = compose' [sum, map (^2), filter even]


-- Exercise 9
curry' :: ((a,b) -> c) -> a -> b -> c
-- curry' f x = (\y -> f (x,y))
curry' f = \x y -> f (x,y)


-- Exercise 10
uncurry' :: (a -> b -> c) -> (a, b) -> c
uncurry' f = \(x,y) -> f x y


-- Exercise 11
unfold :: (b -> Bool) -> (b -> a) -> (b -> b) -> b -> [a]
unfold p h t x
	| p x = []
	| otherwise = h x : unfold p h t (t x)

type Bit = Int

int2bin :: Int -> [Bit]
-- int2bin 0 = []
-- int2bin n = n `mod` 2 : int2bin (n `div` 2)
int2bin = unfold (==0) (`mod` 2) (`div` 2)

chop8 :: [Bit] -> [[Bit]]
-- chop8 [] = []
-- chop8 bits = take 8 bits : chop8 (drop 8 bits)
chop8 = unfold null (take 8) (drop 8)


-- Exercise 12
map' :: (a -> b) -> [a] -> [b]
map' f = unfold null (f . head) tail


-- Exercise 13
iterate_3 :: (a->a) -> a -> [a]
iterate_3 f = unfold (const False) id f


-- Exercise 14
f14 :: (c -> d) -> (b -> c) -> (a -> b) -> (a -> d)
-- f14 f g h = f . (g . h)
f14 f g h = (f . g) . h


-- Exercise 15
f15 x = x : []


-- Exercise 18
f18_1 xs ys = reverse (xs ++ ys)
f18_2 xs ys = reverse ys ++ reverse xs 

-- A higher-order function is a function that takes other functions as arguments or returns a function as result.
-- Currying is the process of transforming a function that takes multiple arguments into a function that takes just a single argument and returns another function if any arguments are still needed.
-- A type that contains one or more class constraints is called overloaded
	

