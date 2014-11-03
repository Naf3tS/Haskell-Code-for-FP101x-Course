-- (1)
-- double (double 2)
-- (double 2) + (double 2)
-- (2 + 2) + (2 + 2)
-- 4 + 4
-- 8
-- or
-- double (double 2)
-- doube (2 + 2)
-- doube 4
-- 4 + 4
-- 8

-- (2)
-- sum [x] 
-- = x + sum[]
-- = x + []
-- = x

-- (3)
product' [] = 1
product' (x:xs) = x * product' xs

-- product [2,3,4]
-- = 2 * product [3,4]
-- = 2 * 3 * product [4]
-- = 2 * 3 * 4 * product []
-- = 2 * 3 * 4 * 1
-- = 24

-- (4)
qsort' [] = []
qsort' (x:xs) = qsort' larger ++ [x] ++ qsort' smaller
	            where
	             	smaller = [a | a<-xs, a<=x]
	             	larger = [b | b<-xs, b>x]

-- (5)
-- Replacing <= with < will drop duplicates!
qsort2' [] = []
qsort2' (x:xs) = qsort2' smaller ++ [x] ++ qsort2' larger
	             where
	             		smaller = [a | a<-xs, a<x]
	             		larger = [b | b<-xs, b>x]