double x = x + x

sum' [] = 0
sum' (x:xs) = x + sum' xs

qsort' [] = []
qsort' (x:xs) = qsort' smaller ++ [x] ++ qsort' larger
	            where
	             	smaller = [a | a<-xs, a<=x]
	             	larger = [b | b<-xs, b>x]

last1' xs = drop (length xs -1 ) xs
last2' xs = head (drop (length xs - 1) xs)
last3' xs = tail (reverse xs)
last4' xs = xs !! (length xs - 1)
last5' xs = head (drop (length xs) xs)

qsort2' [] = []
qsort2' (x:xs) = reverse (qsort2' smaller ++ [x] ++ qsort2' larger)
	where
		smaller = [a | a <- xs, a<=x]
		larger = [b | b <- xs, b>x]

qsort3' [] = []
qsort3' (x:xs) = qsort3' larger ++ qsort3' smaller ++ [x]
	where
		x = minimum xs
		smaller = [a | a <- xs, a<=x]
		larger = [b | b <- xs, b>x]