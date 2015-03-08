-- ========
-- Homework
-- ========
-- use ++ operation for list combination
-- https://wiki.haskell.org/99_questions/1_to_10

-- Exerciese 1
toDigits :: Integer -> [Integer]
toDigits n 
    | n <= 0 = []
    | n < 10 = [n]
    | otherwise = toDigits (n `div` 10) ++ [n `mod` 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev n 
    | n <= 0 = []
    | n < 10 = [n]
    | otherwise = n `mod` 10 : toDigitsRev (n `div` 10)

-- Exercise 2
--
doubleEveryOtherEvenList = zipWith ($) (cycle [(*2),id])

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther n
    | length n `mod` 2 == 0 = doubleEveryOtherEvenList n
    | otherwise = (head n) : doubleEveryOtherEvenList (tail n)

-- Exercise 3

-- sumDigitInteger :: Integer -> Integer
-- sumDigitInteger = sum . toDigits

sumDigits :: [Integer] -> Integer
sumDigits = sum . map (sum . toDigits) -- sumDigitInteger

-- Exercise 4
-- http://stackoverflow.com/questions/4553405/how-can-i-bind-the-second-argument-in-a-function-but-not-the-first-in-an-elegan
-- http://www.freeformatter.com/credit-card-number-generator-validator.html
validate :: Integer -> Bool 
validate = (==0) . flip mod 10  . sumDigits . doubleEveryOther . toDigits

-- Exercise 5
-- Move n discs from peg a to peg b using peg c as temporary storage.
-- 1. Move n-1 discs from a to c using b as temporary storage.
-- 2. Move the top disc from a to b.
-- 3. Move n-1 discs from c to b using a as temporary storage.

type Peg  = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n a b c 
    | n == 0 = []
    | n == 1 = [ (a, b) ]
    | otherwise = (hanoi (n-1) a c b) ++ [(a, b)] ++ (hanoi (n-1) c b a)

-- display number to move.
-- mapM_ print ( shanoi 5 "a" "b" "c" )

type SMove = (Integer, Peg, Peg)
shanoi :: Integer -> Peg -> Peg -> Peg -> [SMove]
shanoi n a b c 
    | n <= 0 = []
    | n == 1 = [ (n, a, b) ]
    | otherwise = shanoi (n-1) a c b ++ [(n, a, b)] ++ shanoi (n-1) c b a

-- Exercise 6 (Optional)
--
--
