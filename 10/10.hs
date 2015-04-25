{-# OPTIONS_GHC -Wall #-}

import AParser

import Control.Applicative
import Data.Char

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
-- Applicative inerface only to define new Parser
{-
(a -> b -> (a, b)) <$> Parser a :: Parser (b -> (a, b))

runParser abParser  "abcdef"
runParser abParser  "aecdef"
runParser abParser_ "abcdef"
runParser abParser_ "aecdef"
runParser intPair "12 34"

-}
abParser :: Parser (Char, Char)
abParser = (\a b -> (a, b))  <$> char 'a' <*> char 'b'

abParser_ :: Parser ()
abParser_ = (\_ _ -> ())     <$> char 'a' <*> char 'b'

intPair :: Parser [Integer]
intPair = (\a _ b -> [a, b]) <$> posInt <*> char ' ' <*> posInt

-- exercise 4
{-
(<|>) is intended to represent choice.
 -}
--class Applicative f => Alternative f where
--	empty :: f a
--	(<|>) :: fa -> f a -> f a

instance Alternative Parser where
	empty = Parser (\_ -> Nothing)
	-- use Maybe's Alternative instance
	Parser f <|> Parser g = Parser h
		where h xs = f xs <|> g xs

-- exercise 5
{-
	runParser intOrUppercase "342abcd"
	runParser intOrUppercase "XYZ"
	runParser "foo"
 -}
intOrUppercase :: Parser ()
intOrUppercase = (g <$> posInt) <|> (g <$> (satisfy isUpper))
	where g _ = ()
