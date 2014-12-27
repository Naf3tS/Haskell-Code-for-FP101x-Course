data Answer = Yes | No | Unknown

answers :: [Answer]
answers = [Yes,No,Unknown]

flip :: Answer -> Answer
flip Yes = No
flip No = Yes
flip Unknown = Unknown


data Shape = Circle Float | Rect Float Float

square :: Float -> Shape
square n = Rect n n

area :: Shape -> Float
area (Circle r) =   pi * r^2
area (Rect x y) = x * y


data Maybe' a = Nothing' | Just' a

safediv _ 0 = Nothing'
safediv m n = Just' (m `div` n)

safehead :: [a] -> Maybe' a
safehead [] = Nothing'
safehead xs = Just' (head xs)


data Nat = Zero | Succ Nat

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n-1))

add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ m) n = Succ (add m n)


data Expr = Val Int
    | Add Expr Expr
    | Mul Expr Expr

size :: Expr -> Int
size (Val n) = 1
size (Add x y) = size x + size y
size (Mul x y) = size x * size y

eval :: Expr -> Int
eval (Val n) = n
eval (Add x y) = eval x + eval y
eval (Mul x y) = eval x * eval y


data Tree = Leaf Int
    | Node Tree Int Tree

occurs :: Int -> Tree -> Bool
occurs m (Leaf n) = m==n
occurs m (Node l n r)
    | m==n = True
    | m<n = occurs m l
    | m>n = occurs m r

flatten :: Tree -> [Int]
flatten (Leaf n) = [n]
flatten (Node l n r) = flatten l ++ [n] ++ flatten r

atree = Node    (Node (Leaf 1) 3 (Leaf 4))
                5
                (Node (Leaf 6) 7 (Leaf 9))