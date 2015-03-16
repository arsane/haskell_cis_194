{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances #-}

import Parser
import StackVM

-- Exercise 5
class Expr a where
    mul :: a -> a -> a 
    add :: a -> a -> a 
    lit :: Integer -> a 

instance Expr Program where
    add a b     = a ++ b ++ [Add]
    mul a b     = a ++ b ++ [Mul]
    lit n       = [PushI n]

compile :: String -> Maybe Program
compile = parseExp lit add mul
