module Book6 where

import Data.Char

twice :: (a -> a) -> a -> a
twice f x = f (f x)

-- The foldr function (fold right):
sum' = foldr (+) 0 -- foldr is only partially applied! Parameter x still needs to be specified.
product' = foldr (*) 1
or' = foldr (||) False
and' = foldr (&&) True

length' = foldr (\_ n -> 1+n) 0

snoc x xs = xs ++ [x]
reverse' = foldr snoc []

-- Use foldl (fold left):
sum'' = foldl (+) 0


-- Composition operator
sumsqreven :: Integral a => [a] -> a
sumsqreven ns = sum (map (^2) (filter even ns))
sumsqreven' :: Integral a => [a] -> a
sumsqreven' = sum . map (^2) . filter even

-- String transmitter
type Bit = Int

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

encode :: String -> [Bit]
encode = concat . map (make8 . int2bit . ord)

chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)

decode :: [Bit] -> String
decode = map (chr . bit2int) . chop8

channel :: [Bit] -> [Bit]
channel = id

transmit :: String -> String
transmit = decode . channel . encode