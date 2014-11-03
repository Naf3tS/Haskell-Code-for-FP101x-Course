e0 = [False,True,False,True]
e1 = [[1,2],[3,4]]

f1 :: Num t => [[t]] -> Char
f1 x = 'x'

e2 = [[[1,2,3]],[[3,4,5]]]

f2 :: Eq t => [[[t]]] -> Char
f2 x = 'x'

e3 x = x * 2

e6 x y = x * y

e7 (x, y) = (y, x)

e11 = ('\a',True)

-- e13 x y = x + y * y
e13 x y = x / y