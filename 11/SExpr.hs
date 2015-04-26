{-# OPTIONS_GHC -Wall #-}
{- CIS 194 HW 11
   due Monday, 8 April
-}

module SExpr where

import AParser
import Control.Applicative
import Data.Char

------------------------------------------------------------
--  1. Parsing repetitions
------------------------------------------------------------
{-
String -> Maybe ([a], String)

fmap :: (a -> b) -> [a] -> [b]
<*>  ::  [a->b]  -> [a] -> [b]

(String -> Maybe (a, String)) -> (String -> Maybe ([a], String))
[a] ++ zeroOrMore p | pure []

runParser (zeroOrMore (satisfy isUpper)) "ABCdEfgH"
runParser (oneOrMore  (satisfy isUpper)) "ABCdEfgH"
runParser (zeroOrMore (satisfy isUpper)) "abcdEfgH"
runParser (oneOrMore  (satisfy isUpper)) "abcdEfgH"
 -}

-- actually zeroOrMore can use oneOrMore to impl;
-- as for cross reference.
zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = (liftA (:) p <*> zeroOrMore p) <|> pure []

oneOrMore :: Parser a -> Parser [a]
oneOrMore p  =  liftA (:) p <*> zeroOrMore p

------------------------------------------------------------
--  2. Utilities
------------------------------------------------------------
{-
runParser ident "foobar baz"
runParser ident "foo33fA"
runParser ident "abad" -- Nothing
runParser ident ""
-}

spaces :: Parser String
spaces = zeroOrMore (satisfy isSpace)

ident :: Parser String
ident = liftA (:) (satisfy isAlpha) <*> (zeroOrMore (satisfy isAlphaNum))

------------------------------------------------------------
--  3. Parsing S-expressions
------------------------------------------------------------

-- An "identifier" is represented as just a String; however, only
-- those Strings consisting of a letter followed by any number of
-- letters and digits are valid identifiers.
type Ident = String

-- An "atom" is either an integer value or an identifier.
data Atom = N Integer | I Ident
  deriving Show

-- An S-expression is either an atom, or a list of S-expressions.
data SExpr = A Atom
           | Comb [SExpr]
  deriving Show

{-
(*>) :: Applicative f => f a -> f b -> f b
(<*) :: Applicative f => f a -> f b -> f a

-- fixme: make space is mandatory between atoms; optional between '(' | ')' and atom.
-}

parseAtom :: Parser Atom 
parseAtom = ((liftA N posInt) <|> (liftA I ident)) <* spaces

parseSExpr :: Parser SExpr
parseSExpr = (liftA A parseAtom) <|> (liftA Comb (spaces *> char '(' *> spaces *> oneOrMore parseSExpr <* spaces <* char ')' <* spaces))
