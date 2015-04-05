{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances #-}

module GuestList where

import Data.List
import Data.Monoid
import Data.Tree
import Employee

-- exercise 1

instance Monoid GuestList where
    mempty      = GL [] 0
    mappend (GL xs xf) (GL ys yf) = GL (xs++ys) (xf+yf)

glCons :: Employee -> GuestList -> GuestList
glCons e gl = (GL [e] (empFun e)) <> gl

-- dump way
moreFun1 :: GuestList -> GuestList -> GuestList
moreFun1 x@(GL _ f1) y@(GL _ f2) | f1 > f2   = x
                                | otherwise = y
-- Ord GuestList
moreFun :: GuestList -> GuestList -> GuestList
moreFun = max

-- exercise 2 

treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node x xs) = f x (map (treeFold f) xs)

-- exercise 3
-- instance (Monoid a, Monoid b) => Monoid (a, b)

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel e gs  = nextgl e $ foldr (\x y -> x <> y) mempty gs
					where nextgl em (sx, sy) = (glCons em sy, sx)

-- exercise 4

-- maxFun testCompany
-- maxFun testCompany2

maxFun :: Tree Employee -> GuestList
maxFun = (uncurry moreFun) . treeFold nextLevel

-- exercise 5

getNames :: GuestList -> ([Name], Fun)
getNames (GL xs f) = (sort $ map empName xs, f)

-- read company's hierarchy from file company.txt and then prints out
-- a formatted guest list, sorted by first name.
-- Total fun: xxxx
-- Adam Debergues
-- xxx xxxx 

main :: IO ()
main = do
	f <- readFile "company.txt"
	let guestlist = getNames (maxFun (read f))
	putStrLn ("Total fun: " ++ show (snd guestlist))
	mapM_ putStrLn (fst guestlist)
