{- CIS 194 HW 11
   due Monday, 8 April
-}

module SExpr where

import AParser
import Control.Applicative

------------------------------------------------------------
--  1. Parsing repetitions
------------------------------------------------------------
{-
String -> Maybe ([a], String)

fmap :: (a -> b) -> [a] -> [b]
<*>  ::  [a->b]  -> [a] -> [b]

(String -> Maybe (a, String)) -> (String -> Maybe ([a], String))
[a] ++ zeroOrMore p | pure []

runParser (zeroOrMore (satify isUpper)) "ABCdEfgH"
runParser (oneOrMore  (satify isUpper)) "ABCdEfgH"
runParser (zeroOrMore (satify isUpper)) "abcdEfgH"
runParser (oneOrMore  (satify isUpper)) "abcdEfgH"
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

spaces :: Parser String
spaces = undefined

ident :: Parser String
ident = undefined

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
