import ExprT
import Parser

-- Exercise-1

eval :: ExprT -> Integer
eval (Lit x) = x
eval (Mul xTree yTree) = (eval xTree) * (eval yTree)
eval (Add xTree yTree) = (eval xTree) + (eval yTree)

-- Exercise-2

evalStr :: String -> Maybe Integer
evalStr s
  | exp == Nothing = Nothing
  | otherwise = Just (eval expr)
  where exp = parseExp Lit Add Mul s 
        Just expr = exp

-- Exercise-3

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit x = Lit x
  add x y = Add x y
  mul x y = Mul x y


 
