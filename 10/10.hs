{-# OPTIONS_GHC -Wall #-}

import AParser

import Control.Applicative

-- exercise 1

first :: (a -> b) -> (a, c) -> (b, c)
first f (x, y) = (f x, y)

instance Functor Parser where
	fmap f (Parser g) = Parser h
		where h xs = case g xs of
			Nothing 		-> Nothing
			Just (y, str) 	-> Just (f y, str)

-- exercise 2
-- to be improved
instance Applicative Parser where
	pure  a = Parser h where h str = Just (a, str)
	(Parser f) <*> (Parser g) = Parser h
		where h xs = case f xs of
			Nothing 	  -> Nothing
			Just (y, str) -> case g str of
				Nothing   		-> Nothing
				Just (z, str') 	-> Just (y z, str')

-- exercise 3

-- abParser :: Parser (Char, Char)
-- abParser = char 'a' <*> char 'b'