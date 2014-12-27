------------------------------------------------------------------------------------------------------------------------------
-- ROSE TREES, FUNCTORS, MONOIDS, FOLDABLES
------------------------------------------------------------------------------------------------------------------------------

data Rose a = a :> [Rose a] deriving Show

-- ===================================
-- Ex. 0-2
-- ===================================

root :: Rose a -> a 
root (x :> xs) = x

children :: Rose a -> [Rose a]
children (x :> xs) = xs

tree1 = 'x' :> map (flip (:>) []) ['a'..'x']
tree2 = 'x' :> map (\c -> c :> []) ['a'..'A']
tree3 = 1 :> map (\c -> c :> []) [1..5]
tree4 = 1 :> [2 :> [], 3 :> [4 :> []]]
tree4' = fmap Product tree4
tree5 = 42 :> [3 :> [2 :> [], 1 :> [0 :> []]]]

xs = 0 :> [1 :> [2 :> [3 :> [4 :> [], 5 :> []]]], 6 :> [], 7 :> [8 :> [9 :> [10 :> []], 11 :> []], 12 :> [13 :> []]]]

ex2 = root . head . children . head . children . head . drop 2 $ children xs

-- ===================================
-- Ex. 3-7
-- ===================================

size :: Rose a -> Int
size (x :> []) = 1
size (x :> xs) = 1 + (sum $ map size xs)

leaves :: Rose a -> Int
leaves (x :> []) = 1
leaves (x :> xs) = sum $ map leaves xs

ex7 = (*) (leaves . head . children . head . children $ xs) (product . map size . children . head . drop 2 . children $ xs)

-- ===================================
-- Ex. 8-10
-- ===================================

instance Functor Rose where
  fmap f (x :> [])  = (f x) :> []
  fmap f (x :> xs) = (f x) :> (map (fmap f) xs)

-- ff :: Rose a -> Rose [a]
ff :: Rose a -> Rose a
-- ff :: Rose a -> [Rose a]
ff r = fmap head $ fmap (\x -> [x]) r

ex10 = round . root . head . children . fmap (\x -> if x > 0.5 then x else 0) $ fmap (\x -> sin(fromIntegral x)) xs

-- ===================================
-- Ex. 11-13
-- ===================================

class Monoid m where
  mempty :: m
  mappend :: m -> m -> m

newtype Sum a = Sum a
newtype Product a = Product a

instance Num a => Monoid (Sum a) where
  mempty = Sum 0
  mappend m1 m2 = Sum (unSum m1 + unSum m2)
  
instance Num a => Monoid (Product a) where
  mempty = Product 1
  mappend m1 m2 = Product (unProduct m1 * unProduct m2)

unSum :: Sum a -> a
unSum (Sum x) = x
unProduct :: Product a -> a
unProduct (Product x) = x

mmm :: Num string => Sum string
mmm = Sum 3 `mappend` Sum 4

num1 = mappend (mappend (Sum 2) (mappend (mappend mempty (Sum 1)) mempty)) (mappend (Sum 2) (Sum 1))
  
num2 = mappend (Sum 3) (mappend mempty (mappend (mappend (mappend (Sum 2) mempty) (Sum (-1))) (Sum 3)))
  
ex13 = unSum (mappend (Sum 5) (Sum (unProduct (mappend (Product (unSum num2)) (mappend (Product (unSum num1)) (mappend mempty (mappend (Product 2) (Product 3))))))))

-- ===================================
-- Ex. 14-15
-- ===================================

class Functor f => Foldable f where
  fold :: Monoid m => f m -> m
  foldMap :: Monoid m => (a -> m) -> (f a -> m)
  foldMap f xs = fold (fmap f xs)
  
instance Foldable Rose where
  fold (x :> []) = x
  fold (x :> xs) = mappend x (foldr (mappend . fold) mempty xs)
  
sumxs = Sum 0 :> [Sum 13 :> [Sum 26 :> [Sum (-31) :> [Sum (-45) :> [], Sum 23 :> []]]], Sum 27 :> [], Sum 9 :> [Sum 15 :> [Sum 3 :> [Sum (-113) :> []], Sum 1 :> []], Sum 71 :> [Sum 55 :> []]]]

ex15 = unSum (mappend (mappend (fold sumxs) (mappend (fold . head . drop 2 . children $ sumxs) (Sum 30))) (fold . head . children $ sumxs))

-- ===================================
-- Ex. 16-18
-- ===================================

ex17 = unSum (mappend (mappend (foldMap (\x -> Sum x) xs) (mappend (foldMap (\x -> Sum x) . head . drop 2 . children $ xs) (Sum 30))) (foldMap (\x -> Sum x) . head . children $ xs))

ex18 = unSum (mappend (mappend (foldMap (\x -> Sum x) xs) (Sum (unProduct (mappend (foldMap (\x -> Product x) . head . drop 2 . children $ xs) (Product 3))))) (foldMap (\x -> Sum x) . head . children $ xs))

-- ===================================
-- Ex. 19-21
-- ===================================

fproduct, fsum :: (Foldable f, Num a) => f a -> a
fsum f = unSum (foldMap Sum f)
fproduct f = unProduct (foldMap Product f)

ex21 = ((fsum . head . drop 1 . children $ xs) + (fproduct . head . children . head . children . head . drop 2 . children $ xs)) - (fsum . head . children . head . children $ xs)



foldl1' f a bs = foldr (\b -> \g -> (\a -> g (f a b))) id bs a
foldl2' f a bs = foldr (\a b -> f b a) a bs
foldl3' f = flip $ foldr (\a b g -> b (f g a)) id
foldl4' = foldr . flip