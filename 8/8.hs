{-# OPTIONS_GHC -Wall #-}

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
moreFun x@(GL _ f1) y@(GL _ f2) | f1 < f2   = x
                                | otherwise = y

-- exercise 2 

treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node x []) = f x []
treeFold f (Node x xs) = f x (map (treeFold f) xs)

-- exercise 3

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)

-- exercise 4

-- maxFun :: Tree Employee -> GuestList


-- exercise 5

-- read company's hierarchy from file company.txt and then prints out
-- a formatted guest list, sorted by first name.
-- Total fun: xxxx
-- Adam Debergues
-- xxx xxxx 

-- main :: IO ()


