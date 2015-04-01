{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances #-}

module JoinListBuffer where

import Data.Monoid
import Sized
import Scrabble
import Buffer

data JoinList m a = Empty
  |   Single m a
  |   Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

-- exercise 1

-- append function

tag :: Monoid m => JoinList m a -> m
tag Empty          = mempty
tag (Single m _)   = m
tag (Append m _ _) = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) j1 j2 = Append ((tag j1) <> (tag j2)) j1 j2

-- exercise 2

-- get the nth element of a list
indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty          = Nothing
indexJ _ (Single _ x)   = Just x
indexJ n (Append m j1 j2)
  | n < 0                     = Nothing
  | n >= (getSize . size $ m) = Nothing
  | n <  (getSize . size . tag $ j1)  = indexJ n j1
  | otherwise                         = indexJ (n - (getSize . size . tag $ j1)) j2

-- convert a JoinList to a list
jtToList :: JoinList m a -> [a]
jtToList Empty              = []
jtToList (Single _ a)       = [a]
jtToList (Append _ l1 l2)   = jtToList l1 ++ jtToList l2

-- drops the first n elements from a JoinList
dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty           = Empty
dropJ 1 (Single _ _)    = Empty
dropJ _ jt@(Single _ _) = jt
dropJ n jt@(Append m j1 j2)
  | n < 0                             = jt
  | n >= (getSize . size $ m)         = Empty
  | n <  (getSize . size . tag $ j1)  = (dropJ n j1) +++ j2
  | otherwise                         = dropJ (n - (getSize . size . tag $ j1)) j2

-- take the first n elements of the a JoinList
takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty           = Empty
takeJ 0 _               = Empty
takeJ _ jt@(Single _ _) = jt
takeJ n jt@(Append m j1 j2)
  | n < 0                             = Empty
  | n >= (getSize . size $ m)         = jt
  | n <  (getSize . size . tag $ j1)  = takeJ n j1
  | otherwise                         = j1 +++ (takeJ (n - (getSize . size . tag $ j1)) j2)

-- exercise 3
-- scoreLine "yay " +++ scoreLine "haskell!"
scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s

-- exercise 4
-- add element at the list head
addJ :: (Sized b, Monoid b) => (a -> b) -> a -> JoinList b a -> JoinList b a
addJ f x Empty              = (Single (f x) x)
addJ f x jt@(Single _ _)    = (Single (f x) x) +++ jt
addJ f x jt@(Append _ j1 j2)
    | (getSize . size . tag $ j1) == (getSize . size . tag $ j2) = (Single (f x) x) +++ jt
    | otherwise             = (addJ f x j1) +++ j2

instance Buffer (JoinList (Score, Size) String) where
  toString           = init . unlines . jtToList
  fromString      s  = foldr (addJ (\x -> (scoreString x, Size 1))) Empty (lines s)
  line               = indexJ
  replaceLine n l b  = takeJ n b +++ fromString l +++ dropJ (n + 1) b
  numLines           = getSize  . snd . tag
  value              = getScore . fst . tag

