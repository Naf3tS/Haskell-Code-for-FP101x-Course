import Data.Char

-- Exercises
-- 1
-- [f x | x <- xs, p x]
exer1 :: (a -> b) -> (a -> Bool) -> [a] -> [b]
exer1 f p xs = [f x | x<-xs, p x]

exer1' :: (a -> b) -> (a -> Bool) -> [a] -> [b]
exer1' f p = map f . (filter p)

-- 2
all' :: [Bool] -> Bool
all' = foldr (&&) True

any' :: [Bool] -> Bool
any' = foldr (||) False

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' f (x:xs)
	| f x = x : takeWhile' f xs
	| otherwise = []

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' f (x:xs)
	| f x = dropWhile' f xs
	| otherwise = x:xs

--3 
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x v -> f x : v) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' f = foldr (\x v -> if (f x) then x:v else v) []

-- 4
-- dec2int [2,3,4,5] =  (2*1000 + (3*100 + (4*10 + (5*1)))) = 2345
dec2int :: [Int] -> Int
dec2int = foldl (\v x -> x + 10*v) 0

-- 5
-- The compose operator needs to be right-associative

-- 6
curry' :: ((a,b) -> c) -> a -> b -> c
curry' f x = (\y -> f (x,y))

doublePair :: Num a => (a,a) -> (a,a)
doublePair (x,y) = (2*x, 2*y)

doublePair10 = curry' doublePair 10


uncurry' :: (a -> b -> c) -> ((a,b)->c)
uncurry' f = \(x,y) -> f  x y

addThem :: Num a => a -> a -> a
addThem x y = x + y

addThemPair = uncurry' addThem


-- 7
-- p Predicate
-- h Function on head
-- t Function on tail
-- x 
unfold p h t x
	| p x = []
	| otherwise = h x : unfold p h t (t x)


type Bit = Int

int2bin' :: Int -> [Bit]
int2bin' = unfold (==0) (`mod` 2) (`div` 2)

chop8' :: [Bit] -> [[Bit]]
chop8' = unfold (null) (take 8) (drop 8)

map'' :: (a -> b) -> [a] -> [b]
map'' f = unfold (null) (f . head) (tail) 

iterate' :: (a -> a) -> a -> [a]
iterate' f x = x : iterate' f (f x)


-- 8
-- String transmitter
bit2int :: [Bit] -> Int
-- bit2int bits =	sum [w*b | (w,b) <- zip weights bits]
-- 				where weights = iterate (*2) 1
bit2int = foldr (\x v -> x + 2*v) 0

int2bit :: Int -> [Bit]
int2bit 0 = []
int2bit x = x `mod` 2 : int2bit (x `div` 2)

int2bit2int = bit2int . int2bit

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

calcParity :: [Bit] -> Bit
calcParity = (`mod` 2) . sum

addParity :: [Bit] -> [Bit]
addParity xs = xs ++ [calcParity xs]

encode :: String -> [Bit]
encode = concat . map (addParity . make8 . int2bit . ord)

chop9 :: [Bit] -> [[Bit]]
chop9 [] = []
chop9 bits = take 9 bits : chop9 (drop 9 bits)

checkParity :: [Bit] -> [Bit]
checkParity bits 
	| (dataParity == calcParity dataNoParity) = dataNoParity
	| otherwise = error "Incorrect parity!"
	where 
		dataNoParity = take 8 bits
		dataParity = last bits

decode :: [Bit] -> String
decode = map (chr . bit2int . checkParity) . chop9

channel :: [Bit] -> [Bit]
channel = id

transmit :: String -> String
transmit = decode . channel . encode

channelLossy :: [Bit] -> [Bit]
channelLossy = tail 

transmitLossy :: String -> String
transmitLossy = decode . channelLossy	 . encode
