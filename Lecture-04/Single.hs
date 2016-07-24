-- Chapter-4 Exercises

-- Exercise-1
import Data.List (foldl')

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

data Tree a = Leaf | Node Integer (Tree a) a (Tree a) deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldl addToTree Leaf

heightOfTree :: Tree a -> Integer
heightOfTree Leaf = -1
heightOfTree (Node h _ _ _) = h

addToTree :: Tree a -> a -> Tree a
addToTree Leaf x = Node 0 Leaf x Leaf
addToTree (Node height lTree node rTree) x
  | hRight < hLeft = Node height lTree node (addToTree rTree x)
  | otherwise = Node (height + 1) (addToTree lTree x) node rTree
  where hRight = heightOfTree rTree
        hLeft = heightOfTree lTree

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

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map (\a -> 2*a+1) $ filter (\x -> not $ x `elem` compositeList) [1..n]
  where ordPair = cartProd [1..n] [1..n]
        compositeList = map (\(x,y) -> x + y + 2*x*y) . filter (\(i,j) -> (1 <= i) && (i <= j) && ((i+j+2*i*j) <= n)) $ ordPair
