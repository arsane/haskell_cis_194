{-# OPTIONS_GHC -Wall #-}

import Data.Monoid
import Sized
import Scrabble

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
jlToList :: JoinList m a -> [a]
jlToList Empty              = []
jlToList (Single _ a)       = [a]
jlToList (Append _ l1 l2)   = jlToList l1 ++ jlToList l2

-- drops the first n elements from a JoinList
dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty           = Empty
dropJ 1 (Single _ _)    = Empty
dropJ _ jl@(Single _ _) = jl
dropJ n jl@(Append m j1 j2)
    | n < 0                             = jl
    | n >= (getSize . size $ m)         = Empty
    | n <  (getSize . size . tag $ j1)  = (dropJ n j1) +++ j2
    | otherwise                         = dropJ (n - (getSize . size . tag $ j1)) j2

-- take the first n elements of the a JoinList
takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty           = Empty
takeJ 0 _               = Empty
takeJ _ jl@(Single _ _) = jl
takeJ n jl@(Append m j1 j2)
    | n < 0                             = Empty
    | n >= (getSize . size $ m)         = jl
    | n <  (getSize . size . tag $ j1)  = takeJ n j1
    | otherwise                         = j1 +++ (takeJ (n - (getSize . size . tag $ j1)) j2)

-- exercise 3
-- scoreLine "yay " +++ scoreLine "haskell!"
scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s

-- exercise 4

