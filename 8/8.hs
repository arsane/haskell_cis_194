{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances #-}

module GuestList where

import Data.Monoid
import Data.Tree
import Employee

-- exercise 1

instance Monoid GuestList where
    mempty      = GL [] 0
    mappend (GL xs xf) (GL ys yf) = GL (xs++ys) (xf+yf)

glCons :: Employee -> GuestList -> GuestList
glCons e gl = (GL [e] (empFun e)) <> gl

moreFun :: GuestList -> GuestList -> GuestList
moreFun x@(GL _ f1) y@(GL _ f2) | f1 > f2   = x
                                | otherwise = y

-- exercise 2 

treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node x []) = f x []
treeFold f (Node x xs) = f x (map (treeFold f) xs)

-- exercise 3
-- instance (Monoid a, Monoid b) => Monoid (a, b)

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel e [] = (GL [e] (empFun e), mempty)
nextLevel e gs  = nextgl e $ foldr (\x y -> x <> y) mempty gs
					where nextgl em (sx, sy) = (glCons em sy, sx)

-- exercise 4

-- maxFun testCompany
-- maxFun testCompany2

maxFun :: Tree Employee -> GuestList
maxFun = (uncurry moreFun) . treeFold nextLevel

-- exercise 5

-- read company's hierarchy from file company.txt and then prints out
-- a formatted guest list, sorted by first name.
-- Total fun: xxxx
-- Adam Debergues
-- xxx xxxx 

-- main :: IO ()


