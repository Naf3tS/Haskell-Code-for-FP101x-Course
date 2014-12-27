import Data.List
import Data.Char
import Unsafe.Coerce

data Nat = Zero
    | Succ Nat
    deriving Show


nTwo :: Nat
nOne = (Succ Zero)
nTwo = Succ (Succ Zero)
nFive = Succ (Succ (Succ (Succ (Succ Zero))))


-- Exercise 0
natToInteger1 :: Nat -> Integer
natToInteger1 Zero = 0
natToInteger1 (Succ n) = natToInteger1 n + 1

natToInteger2 :: Nat -> Integer
natToInteger2 (Succ n) = natToInteger2 n + 1
natToInteger2 Zero = 0

-- natToInteger3 :: Nat -> Integer
--natToInteger3 n = natToInteger3 n

natToInteger4 :: Nat -> Integer
natToInteger4 (Succ n) = 1+ natToInteger4 n
natToInteger4 Zero = 0

natToInteger5 :: Nat -> Integer
natToInteger5 Zero = 1
natToInteger5 (Succ n) = (1 + natToInteger5 n) - 1

natToInteger6 :: Nat -> Integer
natToInteger6  = head . m 
    where   m Zero = [0]
            m (Succ n) = [sum [x | x <- (1:m n)]]

natToInteger7 :: Nat -> Integer
natToInteger7 = \n -> genericLength [c | c <- show n, c=='S']

--natToInteger8 :: Nat -> Integer
--natToInteger8 = \n -> length [c | c <- show n, c=='S']

natToInteger9 :: Nat -> [Char]
natToInteger9 = \n -> [c | c <- show n, c=='S']



-- Exercise 1
integerToNat1 :: Integer -> Nat
integerToNat1 0 = Zero
integerToNat1 (n+1) = Succ (integerToNat1 n)

integerToNat2 :: Integer -> Nat
integerToNat2 0 = Succ Zero
integerToNat2 n = (Succ (integerToNat2 n))

--integerToNat3 :: Integer -> Nat
--integerToNat3 n = product [(unsafeCoerce c)::Integer | c <- show n]

integerToNat4 :: Integer -> Nat
integerToNat4 n = integerToNat4 n

integerToNat5 :: Integer -> Nat
integerToNat5 (n+1) = Succ (integerToNat5 n)
integerToNat5 0 = Zero

integerToNat6 :: Integer -> Nat
integerToNat6 (n+1) = let m = integerToNat6 n in Succ m
integerToNat6 0 = Zero

--integerToNat7 :: Integer -> Nat
--integerToNat7 = head . m
--    where   {
--            ; m 0 = [0]
--            ; m (n+1) = [sum [x | x<-(1 : m n)]]
--            }

--integerToNat8 :: Integer -> Nat
--integerToNat8 = \n -> genericLength [c | c<-show n, isDigit c]



-- Exercise 2
add1 :: Nat -> Nat -> Nat
add1 Zero n = n
add1 (Succ m) n = Succ (add1 m n)

add2 :: Nat -> Nat -> Nat
add2 (Succ m) n = Succ (add2 n m)
add2 Zero n = n

add3 :: Nat -> Nat -> Nat
add3 Zero n = Zero
add3 (Succ m) n = Succ (add3 m n)

add4 :: Nat -> Nat -> Nat
add4 (Succ m) n = Succ (add4 m n)
add4 Zero n = Zero

add5 :: Nat -> Nat -> Nat
add5 n Zero = Zero
add5 n (Succ m) = Succ (add5 n m)

add6 :: Nat -> Nat -> Nat
add6 n (Succ m) = Succ (add6 n m)
add6 n Zero = Zero

add7 :: Nat -> Nat -> Nat
add7 n Zero = n
add7 n (Succ m) = Succ (add7 m n)

add8 :: Nat -> Nat -> Nat
add8 n (Succ m) = Succ (add8 m n)
add8 n Zero = n



-- Exercise 3
mult :: Nat -> Nat -> Nat
mult m Zero = Zero
mult m (Succ n) = add8 m (mult m n)



-- Exercise 4
data Tree = Leaf Integer | Node Tree Integer Tree
atree = Node    (Node (Leaf 1) 3 (Leaf 4))
                5
                (Node (Leaf 6) 7 (Leaf 9))

occurs1 :: Integer -> Tree -> Bool
occurs1 m (Leaf n) = m==n
occurs1 m (Node l n r) = case compare m n of
    LT -> occurs1 m l
    EQ -> True
    GT -> occurs1 m r

--occurs2 :: Integer -> Tree -> Bool
--occurs3 :: Integer -> Tree -> Bool
--occurs4 :: Integer -> Tree -> Bool

occurs5 :: Integer -> Tree -> Bool
occurs5 m (Leaf n) = m==n
occurs5 m (Node l n r)
    | m==n = True
    | m<n = occurs5 m l
    | otherwise = occurs5 m r

--occurs6 :: Integer -> Tree -> Bool
--occurs7 :: Integer -> Tree -> Bool
--occurs8 :: Integer -> Tree -> Bool



-- Exercise 5
data Tree2 = Leaf2 Integer | Node2 Tree2 Tree2 deriving Show
atree2 = Node2  (Node2 (Leaf2 1) (Leaf2 4))
                (Node2 (Leaf2 6) (Leaf2 9))
atree3 = Node2  (Node2 (Leaf2 1) (Node2 (Node2 (Leaf2 2) (Leaf2 3)) (Leaf2 4)))
                (Leaf2 6)

balanced4 :: Tree2 -> Bool
leaves4 (Leaf2 _) = 1
leaves4 (Node2 l r) = leaves4 l + leaves4 r
balanced4 (Leaf2 _) = True
balanced4 (Node2 l r) = abs(leaves4 l - leaves4 r) <= 1 && balanced4 l && balanced4 r



-- Exercise 6
balance1 :: [Integer] -> Tree2
halve1 xs = splitAt (length xs `div` 2) xs
balance1 [x] = Leaf2 x
balance1 xs = Node2 (balance1 ys) (balance1 zs)
    where (ys, zs) = halve1 xs




-- Exercise 10
data Maybe' a = Nothing' | Just' a

instance Monad Maybe' where
    return x = Just' x
    Nothing' >>= _  = Nothing'
    (Just' x) >>= f = f x



-- Exercise 11
class Monoid' a where
    mempty :: a
    (<>) :: a->a->a

instance Monoid' [a] where
    mempty = []
    (<>) = (++)



-- Exercise 12
class Functor' f where
    fmap :: (a->b) -> f a -> f b

instance Functor' Maybe' where
    fmap _ Nothing' = Nothing'
    fmap f (Just' a) = Just' (f a)


-- Exercise 13
class (Functor' f) => Foldable' f where
    fold :: (Monoid' m) => f m -> m

instance Foldable' [] where
    --fold = foldl (map . (<>)) mempty
    --fold xs  = map (<>) xs
    --fold = foldr (<>) mempty
    fold xs = concat . map (mempty)