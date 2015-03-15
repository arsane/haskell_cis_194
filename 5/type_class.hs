import ExprT
import Parser

eval :: ExprT -> Integer
eval (Lit n)   = n
eval (Add a b) = (eval a) + (eval b)
eval (Mul a b) = (eval a) * (eval b)

evalStr :: String -> Maybe Integer
evalStr str = case expr of
                  Nothing -> Nothing
                  Just e  -> Just $ eval e
              where expr = parseExp Lit Add Mul str
    
class Expr a where
    mul :: a -> a -> a 
    add :: a -> a -> a 
    lit :: Integer -> a 

instance Expr ExprT where
    mul a b = Mul a b
    add a b = Add a b
    lit n   = Lit n
