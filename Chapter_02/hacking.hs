double' x = x+x
quadrupleX x = double' (double' x)

factorial' n = product [1..n]
average' ns = sum ns `div` length ns

-- Layout rule:
a1 = b1 + c1
	where
		b1 = 1
		c1 = 2
d1 = a1 * 2

-- Or:
a2 = b2 + c2
	where
		{b2 = 1;
		 c2 = 2}
d2 = a2 * 2

{-
	This is a nested comment
	It can span multiple lines, hurrah!
-}

second xs = head (tail xs)
swap (x,y) = (y,x)
pair x y = (x,y)
double x = x*2
palindrome xs = reverse xs == xs
twice f x = f (f x)
f xs = take 3 (reverse xs)