-- Chapter-4 Exercises

-- Exercise-1

fun1 :: [Integer] -> Integer
fun1 []     = 1
fun1 (x:xs)
  | even x    = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . map (flip (-) 2) . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
  | even n    = n + fun2 (n `div` 2)
  | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (/=1) . iterate collatz
  where collatz n
          | even n = div n 2
          | otherwise = 3 * n + 1
            
-- Exercise-2 -> Not yet Completed :( 

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
            deriving (Show, Eq)

treeHeight :: Tree a -> Integer
treeHeight Leaf = -1
treeHeight (Node h _ _ _) = h

balancedTree :: Tree a -> Bool
balancedTree Leaf = True
balancedTree (Node h lTree node rTree) = (balancedTree lTree) && (balancedTree rTree) && balHeight
  where balHeight = abs ((treeHeight lTree) - (treeHeight rTree)) <= 1


insert :: a -> Tree a -> Tree a
insert x Leaf = Node 0 Leaf x Leaf
insert x tree@(Node h lTree node rTree)
  | min (treeHeight lTree) (treeHeight rTree) == (treeHeight lTree) =  Node (h+1) (insert x lTree) node rTree
  | otherwise = Node (h+1) lTree node (insert x rTree)

foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf  

-- Exercise-3

-- 1) xor : returns True if and only if there are an odd number of True values
-- contained in the input list. It does not matter how many False values the
-- input list contains

xor :: [Bool] -> Bool
xor = foldl (\a b -> (a && (not b)) || (b && (not a))) False . filter (\x -> x) 

-- 2) Implementing map as fold

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> f x : acc) []

-- 3) Implementing foldl using foldr (Optional)

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base  = foldr (flip f) base . reverse   

-- Exercise-4

-- Sieve of Sundaram for finding primes -> I think this will be interesting

-- cartProd : To produce the cartesian product of two lists

cartProd :: [a] -> [b] -> [(a,b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]
