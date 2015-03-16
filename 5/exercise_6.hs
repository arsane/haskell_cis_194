{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances #-}

import qualified Data.Map as M

class Expr a where
    mul :: a -> a -> a 
    add :: a -> a -> a 
    lit :: Integer -> a 

data VarExprT = Lit Integer
            |   Add VarExprT VarExprT
            |   Mul VarExprT VarExprT
            |   Var String

class HasVars a where
    var :: String -> a

-- make VarExprT an instance of Expr and HasVars
    
instance Expr VarExprT where
    add a b = Add a b
    mul a b = Mul a b
    lit n   = Lit n

instance HasVars VarExprT where
    var s   = Var s

instance HasVars (M.Map String Integer -> Maybe Integer) where
    var s   = M.lookup s

instance Expr (M.Map String Integer -> Maybe Integer) where
    add a b = \x -> maybeAdd (a x) (b x)
                    where maybeAdd Nothing   _         = Nothing
                          maybeAdd _         Nothing   = Nothing
                          maybeAdd (Just n1) (Just n2) = Just (n1 + n2)
    mul a b = \x -> maybeMul (a x)  (b x)
                    where maybeMul Nothing    _        = Nothing
                          maybeMul _          Nothing  = Nothing
                          maybeMul (Just n1) (Just n2) = Just (n1 * n2)
    lit n   = \_ -> Just n
  
withVars :: [(String, Integer)]
            -> (M.Map String Integer -> Maybe Integer)
            -> Maybe Integer
withVars vs expr = expr $ M.fromList vs

x1 :: Maybe Integer
x2 :: Maybe Integer
x3 :: Maybe Integer

x1 = withVars [("x", 6)] $ add (lit 3) (var "x")
x2 = withVars [("x", 6)] $ add (lit 3) (var "y")
x3 = withVars [("x", 6), ("y", 3)] $ mul (var "x") (add (var "x") (var "y"))
