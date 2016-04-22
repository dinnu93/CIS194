module Party where

import Employee
import Data.Tree

-- Exercise-1

-- 1) adds and Employee to the GuestList Naively
glCons :: Employee -> GuestList -> GuestList
glCons emp@(Emp name fun) (GL gList funScore) = GL (emp:gList) (fun + funScore)

-- 2) Monoid instance for GuestList with max operation
instance Monoid GuestList where
  mempty = GL [] 0
  mappend = max 

-- 3) Checks which GuestList is more fun
moreFun :: GuestList -> GuestList -> GuestList
moreFun = max

-- Exercise-2

--treeFold - folds the 'rose trees' in haskell

treeFold :: (a -> b) -> b -> (b -> b -> b) -> (b -> b -> b) -> Tree a -> b
treeFold fempty identity f g (Node a xs) = f (fempty a) (foldr g identity (map (treeFold fempty identity f g) xs)) 

