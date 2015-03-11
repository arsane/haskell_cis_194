{-# OPTIONS_GHC -Wall #-}

-- Exercise 1: Wholemeal programming
-- =================================

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
    | even x = (x - 2) * fun1 xs
    | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = foldl (*) 1 . map (+(-2)) . filter  even

-- use function iterate and takeWhile

-- iterate :: (a -> a) -> a -> [a] Source
-- iterate f x returns an infinite list of repeated applications of f to x:
-- iterate f x == [x, f x, f (f x), ...]

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
    | even n = n + fun2 (n `div` 2)
    | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = sum . takeWhile (/=1) . iterate (\x ->  if (even x) then x `div` 2 else (3*x + 1))

-- Exercise 2: Folding with trees
-- ==============================
-- http://en.wikipedia.org/wiki/Binary_tree
-- generate a balanced tree from a list of values using foldr

data Tree a = Leaf 
            | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)

-- foldTree :: [a] -> Tree a


-- Exercise 3: More folds
-- ======================

xor :: [Bool] -> Bool
xor = foldl (/=) False . filter id

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\a bs -> (f a) : bs) []

-- implement foldl using foldr
-- foldr f z [x1, x2, ..., xn] == x1 `f` (x2 `f` ... (xn `f` z)...)
-- foldl f z [x1, x2, ..., xn] == (...((z `f` x1) `f` x2) `f`...) `f` xn
-- http://stackoverflow.com/questions/6172004/writing-foldl-using-foldr

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (\x g -> (\c -> g (f c x))) id xs base

-- Exercise 4: Finding primes
-- ==========================
-- http://en.wikipedia.org/wiki/Sieve_of_Sundaram

-- Given an integer n, your function should generate all odd prime numbers 
-- up to 2n+2

-- sieveSundaram :: Integer -> [Integer]
-- sieveSundaram = 
