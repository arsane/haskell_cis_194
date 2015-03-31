{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Scrabble where

import Data.Char
import Data.Monoid

newtype Score = Score Int
    deriving (Eq, Ord, Show, Num)

getScore :: Score -> Int
getScore (Score i) = i

instance Monoid Score where
    mempty  = Score 0
    mappend = (+)

-- http://stackoverflow.com/questions/23264230/find-the-score-of-a-scrabble-word-in-haskell

mapper :: Eq k => [(k, v)] -> k -> v
mapper dict k = case lookup k dict of Nothing  -> undefined
                                      (Just v) -> v
score :: Char -> Score
score c 
    | isLetter c = Score $ mapper [ ('A', 1)
                  , ('B', 3)
                  , ('C', 3)
                  , ('E', 1)
                  , ('D', 2)
                  , ('G', 2)
                  , ('F', 4)
                  , ('I', 1)
                  , ('H', 4)
                  , ('K', 5)
                  , ('J', 8)
                  , ('M', 3)
                  , ('L', 1)
                  , ('O', 1)
                  , ('N', 1)
                  , ('Q', 10)
                  , ('P', 3)
                  , ('S', 1)
                  , ('R', 1)
                  , ('U', 1)
                  , ('T', 1)
                  , ('W', 4)
                  , ('V', 4)
                  , ('Y', 4)
                  , ('X', 8)
                  , ('Z', 10)
                  ] . toUpper $ c
    | otherwise = Score 0

scoreString :: String -> Score
scoreString = sum . map score



