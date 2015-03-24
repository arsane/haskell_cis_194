{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# LANGUAGE FlexibleInstances #-}

-- Fibonacci numbers
-- exercise 1

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = (fib (n-1)) + (fib (n-2))

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- exercise 2

fibs2 :: [Integer]
fibs2 = 0:1:next fibs2
	where next (a:t@(b:_)) = (a+b):next t

-- exercise 3

data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons x y) = x:(streamToList y)

-- display the first 20 elements
instance Show a => Show (Stream a) where
	show m = show $ take 40 (streamToList m )
--	show m = show $ zipWith (\x _ -> x) (streamToList m ) [0..19]

-- exercise 4

-- streamRepeat "ABC"
streamRepeat :: a -> Stream a
streamRepeat n = Cons n (streamRepeat n)

-- streamMap (\x -> [1..x]) $ streamRepeat 5
streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x y) = Cons (f x) (streamMap f y)

-- streamFromSeed (\x->x+2) 2
streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f n = Cons n (streamFromSeed f (f n))

-- exercise 5

nats :: Stream Integer
nats = streamFromSeed (\x -> x + 1) 0

-- https://mail.haskell.org/pipermail/beginners/2014-February/013160.html
-- interleaveStreams (streamRepeat 0)
--   (interleaveStreams (streamRepeat 1)
--     (interleaveStreams (streamRepeat 2)
--       (interleaveStreams (streamRepeat 3)
--         ...

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons a as) bs = Cons a (case bs of Cons b bs' -> Cons b (interleaveStreams as bs'))

ruler :: Stream Integer
ruler = foldr1 interleaveStreams (map streamRepeat [0..])

-- exercise 6
z :: Stream Integer
z = Cons 0 (Cons 1 (streamRepeat 0))

-- helper
imulstream :: Integer -> Stream Integer -> Stream Integer
imulstream n (Cons a as) = Cons (n*a) (imulstream n as)

-- Num instance
instance Num (Stream Integer) where
    fromInteger n = Cons n (streamRepeat 0)
    negate (Cons a as) = Cons (negate a) (negate as)    
    (+) (Cons a as) (Cons b bs) = Cons (a + b) (as + bs)
    (*) (Cons a as') bs@(Cons b bs') = Cons (a*b) ((imulstream a bs') + (as'*bs))

-- z^4
-- (1+z)^5
-- (z^2 + z + 3) * (z - 5)

-- Fractional instance
instance Fractional (Stream Integer) where
    (/) as@(Cons a as') bs@(Cons b bs') = Cons (a `div` b) (imulstream (1 `div` b) (as' - ((as/bs)*bs')))

-- Fibonacci numbers
fib3 :: Stream Integer
fib3 = z / (1 - z - z^2)

-- exercise 7

data Matrix = Matrix Integer Integer Integer Integer
        deriving Show
instance Num Matrix where
    (*) (Matrix a0 a1 a2 a3) (Matrix b0 b1 b2 b3) = Matrix c0 c1 c2 c3
                                    where c0 = a0*b0 + a1*b2
                                          c1 = a0*b1 + a1*b3
                                          c2 = a2*b0 + a3*b2
                                          c3 = a2*b1 + a3*b3

-- map fib4 [0..20]
fib4 :: Integer -> Integer
fib4 n = corner $ matrix n 
            where corner (Matrix _ a _ _) = a
                  matrix n
                        | n == 0 = Matrix 0 0 0 0
                        | n == 1 = Matrix 1 1 1 0
                        | even n = (matrix (n `div` 2))^2
                        | odd  n = (matrix 1) * (matrix (n - 1))
