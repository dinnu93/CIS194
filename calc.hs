{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

import ExprT
import Parser
import qualified StackVM as SVM
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

-- Exercise-4

instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)
  
 
instance Expr Bool where
  lit x = if x < 0 then False else True
  add = (||)
  mul = (&&)

newtype MinMax  = MinMax Integer deriving (Eq, Show)
newtype Mod7    = Mod7 Integer deriving (Eq, Show)

instance Expr MinMax where
  lit x = MinMax x
  add (MinMax x) (MinMax y) = MinMax (max x y)
  mul (MinMax x) (MinMax y) = MinMax (min x y)

instance Expr Mod7 where
  lit x = Mod7 (mod x 7)
  add (Mod7 x) (Mod7 y) = Mod7 (mod (x + y) 7)
  mul (Mod7 x) (Mod7 y) = Mod7 (mod (x * y) 7)


-- Exercise-5

instance Expr SVM.Program where
  lit x = [SVM.PushI x]
  add x y =  x ++ y ++ [SVM.Add]
  mul x y = x ++ y ++ [SVM.Mul]

  
