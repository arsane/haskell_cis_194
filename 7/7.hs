{-# OPTIONS_GHC -Wall #-}

import Data.Monoid
import Sized

data JoinList m a = Empty
                |   Single m a
                |   Append m (JoinList m a) (JoinList m a)
    deriving (Eq, Show)

-- exercise 1

-- append function

tag :: Monoid m => JoinList m a -> m
tag Empty            = mempty
tag (Single m _)     = m  
tag (Append m _ _) = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) j1 j2 = Append ((tag j1) <> (tag j2)) j1 j2

-- exercise 2

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a 
indexJ n jl 
    | n < 0 = Nothing
    | n > (getSize . tag jl) = Nothing
    | _ Empty = Nothing

