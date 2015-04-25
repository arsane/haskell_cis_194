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
{-
type Name = String
data Employee = Emp { name :: Name, phone :: String }

parseName  :: Parser Name
parsePhone :: Parser String

then

Emp <$> parseName <*> parsePhone :: Parser Employee

--
-- Maybe Applicative
-- only param 1 and param 2 both Just, then result is Just.
-- otherwise Nothing.
(<*>) :: Maybe (a->b) ->  Maybe a -> Maybe b

-- Parser Applicative
(<*>) :: (String -> Maybe (a->b, String)) ->
	     (String -> Maybe (a, String))    ->
	     (String -> Maybe (b, String))

Maybe (a->b) -> Maybe a -> Maybe b
--
-- Monad
Maybe String -> (String -> Maybe (a, String)) -> Maybe (a, String) -- >>=
(a->b, String) -> Maybe (a, String) -> Maybe (b, String)
Maybe (a->b, String) -> Maybe (a, String) -> Maybe (b, String)
Maybe (a->b) -> Maybe a -> Maybe b
Maybe b -> Maybe String -> Maybe (b, String)
b -> String -> (b, String)
--
(a -> b) -> f a -> f b
(fmap snd)   :: Functor f => f (a, b) -> f b
(fmap first) :: Functor f => f (a, b) -> f a
-}

instance Applicative Parser where
	pure a = Parser h where h str = Just (a, str)
	-- use Maybe Applicative
	(Parser f) <*> (Parser g) = Parser h
		where h xs = (\a b -> (a, b)) <$> ((fst <$> t) <*> (fst <$> u)) <*> (snd <$> u) where
				t = f xs
				u = (snd <$> t)	>>= g

-- exercise 3

-- abParser :: Parser (Char, Char)
-- abParser = char 'a' <*> char 'b'