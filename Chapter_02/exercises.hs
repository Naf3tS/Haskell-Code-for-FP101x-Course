-- (1)
-- (2 ^3 ) * 4
-- (2 * 3) + (4 * 5)
-- 2 + (3 * (4 ^ 5))

-- (2)

-- (3)
{-
N = a `div` length xs
	where
		 a = 10
		xs = [1,2,3,4,5]
-}
n = a `div` length xs
	where
		a = 10
		xs = [1,2,3,4,5]

-- (4)
last' xs = head (reverse xs)

-- (5)
init' xs = take ((length xs)-1) xs