{-# OPTIONS_GHC -Wall #-}

import AParser

import Control.Applicative

-- exercise 1

first :: (a -> b) -> (a, c) -> (b, c)
first f (x, y) = (f x, y)

{-
fmap :: (a -> b) -> Parser a -> Parser b
g :: String -> Maybe (a, String)
h :: String -> Maybe (b, String)

first:: (a -> b ) -> (a, String) -> (b, String)
-}
instance Functor Parser where
	-- use Maybe Functor
	fmap f (Parser g) = Parser h
		where h xs = fmap (first f) (g xs)

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