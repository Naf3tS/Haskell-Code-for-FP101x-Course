module Chapter_08 where

import System.IO
import Data.Char


-- Exercise 1
putStr' :: String -> IO ()
putStr' [] = return ()
putStr' (x:xs) = putChar x >> putStr' xs

putStrLn1 :: String -> IO ()
putStrLn1 [] = putChar '\n'
putStrLn1 xs = putStr' xs >> putStrLn1 ""

putStrLn2 :: String -> IO ()
putStrLn2 [] = putChar '\n'
putStrLn2 xs = putStr' xs >> putChar '\n'

putStrLn3 :: String -> IO ()
putStrLn3 [] = putChar '\n'
putStrLn3 xs = putStr' xs >>= \x -> putChar '\n'

-- putStrLn4 :: String -> IO ()
-- putStrLn4 [] = putChar '\n'
-- putStrLn4 xs = putStr' xs >> \x -> putChar '\n'

putStrLn5 :: String -> IO ()
putStrLn5 [] = putChar '\n'
putStrLn5 xs = putStr' xs >> putStr' "\n"

putStrLn6 :: String -> IO ()
putStrLn6 [] = putChar '\n'
putStrLn6 xs = putStr' xs >> putStrLn6 "\n"

-- putStrLn7 :: String -> IO ()
-- putStrLn7 [] = return ""
-- putStrLn7 xs = putStrLn7 xs >> putStr' "\n"

-- putStrLn8 :: String -> IO ()
-- putStrLn8 [] = putChar "\n"
-- putStrLn8 xs = putStr' xs >> putChar '\n'

putStrLn' :: String -> IO ()
putStrLn' [] = putChar '\n'
putStrLn' xs = putStr' xs >> putStrLn' ""


-- Exercise 3
getLine1 :: IO String
getLine1 = get1 ""
get1 :: String -> IO String
get1 xs = do    x <- getChar
                case x of 
                    ' ' -> return xs
                    '\n' -> return xs
                    _ -> get1 (xs ++ [x]) 

getLine2 :: IO String
getLine2 = get2 ""
get2 :: String -> IO String
get2 xs = do    x <- getChar
                case x of 
                    '\n' -> return xs
                    _ -> get2 (x:xs)

getLine3 :: IO String
getLine3 = get3 []
get3 :: String -> IO String
get3 xs = do    x <- getChar
                case x of 
                    '\n' -> return xs
                    _ -> get3 (xs ++ [x]) 

getLine4 :: IO String
getLine4 = get4 []
get4 :: String -> IO String
get4 xs = do    x <- getChar
                case x of 
                    '\n' -> return (x:xs)
                    _ -> get4 (xs ++ [x]) 

getLine' :: IO String
getLine' = get' []
get' :: String -> IO String
get' xs = do    x <- getChar
                case x of 
                    '\n' -> return xs
                    _ -> get' (xs ++ [x]) 


-- Exercise 4
interact' :: (String -> String) -> IO ()
interact' f = do    input <- getLine'
                    putStrLn' (f input)


-- Simple list of Monad values:
theList :: [IO ()]
theList = [putChar 'a', putChar 'b', putChar '\n']
theList2 = [Just 1, Just 2, Just 3]


-- Exercise 5
-- sequence_1 :: Monad m => [m a] -> m ()
-- sequence_1 [] = return []
-- sequence_1 (m:ms) = m >> \_ -> sequence_1 ms

sequence_2 :: Monad m => [m a] -> m ()
sequence_2 [] = return ()
sequence_2 (m:ms) = (foldl (>>) m ms) >> return ()

-- sequence_3 :: Monad m => [m a] -> m ()
-- sequence_3 ms = foldl (>>) (return ()) ms

sequence_4 :: Monad m => [m a] -> m ()
sequence_4 [] = return ()
sequence_4 (m:ms) = m >> sequence_4 ms

sequence_5 :: Monad m => [m a] -> m ()
sequence_5 [] = return ()
sequence_5 (m:ms) = m >>= \_ -> sequence_5 ms

-- sequence_6 :: Monad m => [m a] -> m ()
-- sequence_6 ms = foldr (>>=) (return ()) ms

sequence_7 :: Monad m => [m a] -> m ()
sequence_7 ms = foldr (>>) (return ()) ms

-- sequence_8 :: Monad m => [m a] -> m ()
-- sequence_8 ms = foldr (>>) (return []) ms


-- Exercise 6
sequence1 :: Monad m => [m a] -> m [a]
sequence1 [] = return []
sequence1 (m:ms) = m >>= \a -> do   as <- sequence1 ms 
                                    return (a:as)

--sequence2 :: Monad m => [m a] -> m [a]
--sequence2 ms = foldr func (return ()) ms
--    where   func :: (Monad m) => m a -> m [a] -> m [a]
--            func m acc = do     x <- m 
--                                xs <- acc
--                                return (x:xs)

--sequence3 :: Monad m => [m a] -> m [a]
--sequence3 ms = foldr func (return []) ms
--    where
--        func :: (Monad m) => m a -> m [a] -> m [a]
--        func m acc = m:acc

--sequence4 :: Monad m => [m a] -> m [a]
--sequence4 [] = return []
--sequence4 (m:ms) = return (a:as)
--    where
--        a <- m
--        as <- sequence4 ms

--sequence5 :: Monad m => [m a] -> m [a]
--sequence5 ms = foldr func (return []) ms
--    where
--        func :: (Monad m) => ma -> m [a] -> m [a]
--        func m acc = do x <- m
--                        xs <- acc
--                        return (x:xs)

--sequence6 :: Monad m => [m a] -> m [a]
--sequence6 []  = return []
--sequence6 (m:ms) = m >> \a -> 
--    do  as <- sequence6 ms
--        return (a:as)

--sequence7 :: Monad m => [m a] -> m [a]
--sequence7 [] = return []
--sequence7 (m:ms) = m >>= \a -> 
--    as <- sequence7 ms
--    return (a:as)

sequence8 :: Monad m => [m a] -> m [a]
sequence8 [] = return []
sequence8 (m:ms) = do   a <- m
                        as <- sequence8 ms
                        return (a:as)


-- Exercise 8
filterM' :: Monad m => (a -> m Bool) -> [a] -> m [a]
-- 1
--filterM' _ [] = return []
--filterM' p (x:xs) = do  flag <- p x
--                        ys <- filterM' p xs
--                        return (x:ys)
-- 2
filterM' _ [] = return []
filterM' p (x:xs) = do  flag <- p x
                        ys <- filterM' p xs
                        if flag then return (x:ys) else return ys
-- 3
--filterM' _ [] = return []
--filterM' p (x:xs) = do  ys <- filterM' p xs
--                        if p x then return (x:ys) else return ys
-- 4
--filterM' _ [] = return []
--filterM' p (x:xs) = do  flag <- p x
--                        ys <- filterM' p xs
--                        if flag then return ys else return (x:ys)


-- Exercise 9
--foldLeftM :: Monad m => (a -> b -> m a) -> a -> [b] -> m a
--foldLeftM f z0 xs0 = lgo z0 xs0
--             where
--                lgo z []     =  z
--                lgo z (x:xs) = lgo (f z x) xs

-- Exercise 11
--liftM1 :: Monad m => (a -> b) -> m a -> m b
--liftM1 f m = do x <- m 
--                return f(x)

--liftM2 :: Monad m => (a -> b) -> m a -> m b
--liftM2 f m = m >>= \a -> f a

liftM3 :: Monad m => (a -> b) -> m a -> m b
liftM3 f m = m >>= \a -> return (f a)

--liftM4 :: Monad m => (a -> b) -> m a -> m b
--liftM4 f m = return (f m)

--liftM5 :: Monad m => (a -> b) -> m a -> m b
--liftM5 f m = m >>= \a -> m >>= \b return (f a)

--liftM6 :: Monad m => (a -> b) -> m a -> m b
--liftM6 f m = m >>= \a -> m >>= \b return (f b)

--liftM7 :: Monad m => (a -> b) -> m a -> m b
--liftM7 f m = mapM f [m]

liftM8 :: Monad m => (a -> b) -> m a -> m b
liftM8 f m = m >>= \a -> return (f a)
