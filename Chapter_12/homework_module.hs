module Chapter12 where

last1 :: [a] -> a
last1 (_:xs) = last1 xs

foldrX :: (a->b->b) -> b -> [a] -> b
foldrX _ v [] = v
foldrX f v (x:xs) = f x (foldrX f v xs)

initX :: [a] -> [a]
initX [_] = []
initX (x:xs) = x : initX xs

dropX :: Int -> [a] -> [a]
dropX 0 xs = xs
dropX n [] = []
dropX n (_:xs) = dropX (n-1) xs

(++**) :: [a] -> [a] -> [a]
[] ++** ys = ys
(x:xs) ++** ys = x : (xs ++** ys)

foldlX :: (a->b->a) -> a -> [b] -> a
foldlX _ v [] = v
foldlX f v (x:xs) = foldlX f (f v x) xs