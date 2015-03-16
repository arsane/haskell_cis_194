{-# OPTIONS_GHC -Wall #-}

import ExprT
import Parser
import qualified Data.Map as M

-- Exercise 1

eval :: ExprT -> Integer
eval (Lit n)   = n
eval (Add a b) = (eval a) + (eval b)
eval (Mul a b) = (eval a) * (eval b)

-- Exercise 2

evalStr :: String -> Maybe Integer
evalStr str = case expr of
                  Nothing -> Nothing
                  Just e  -> Just $ eval e
              where expr = parseExp Lit Add Mul str

-- Exercise 3
class Expr a where
    mul :: a -> a -> a 
    add :: a -> a -> a 
    lit :: Integer -> a 

instance Expr ExprT where
    mul a b = Mul a b
    add a b = Add a b
    lit n   = Lit n

-- reify $ mul (add (lit 2) (lit 3)) (lit 4)
reify :: ExprT -> ExprT
reify = id

-- Exercise 4

instance Expr Integer where
    mul a b = a * b
    add a b = a + b
    lit n   = n

instance Expr Bool where
    mul a b = a && b
    add a b = a || b
    lit a   = a > 0

newtype MinMax = MinMax Integer
    deriving (Eq, Show)

newtype Mod7 = Mod7 Integer
    deriving (Eq, Show)

instance Expr MinMax where
    add (MinMax a) (MinMax b) = MinMax (max a b)
    mul (MinMax a) (MinMax b) = MinMax (min a b)
    lit n   = MinMax n

instance Expr Mod7 where
    add (Mod7 a) (Mod7 b) = Mod7 ((a + b) `mod` 7)
    mul (Mod7 a) (Mod7 b) = Mod7 ((a * b) `mod` 7)
    lit n                 = Mod7 ( n `mod` 7)

-- easy to swap in new semantics for the same syntactic expression.

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExp :: Maybe Integer
testBool    = testExp :: Maybe Bool
testMM      = testExp :: Maybe MinMax
testSat     = testExp :: Maybe Mod7
